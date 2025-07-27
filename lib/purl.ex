defmodule Purl do
  @moduledoc """
  Elixir Implementation of the purl (package url) specification.

  ## Specification

  https://github.com/package-url/purl-spec

  **Format**: `pkg:type/namespace/name@version?qualifiers#subpath`

  > #### License {: .neutral}
  >
  > A lot of the documentation was taken directly from the specification. It is
  > licensed under the MIT License:
  > ```
  > Copyright (c) the purl authors
  >
  > Permission is hereby granted, free of charge, to any person obtaining a copy of
  > this software and associated documentation files (the "Software"), to deal in
  > the Software without restriction, including without limitation the rights to
  > use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
  > the Software, and to permit persons to whom the Software is furnished to do so,
  > subject to the following conditions:
  >
  > The above copyright notice and this permission notice shall be included in all
  > copies or substantial portions of the Software.
  >
  > THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  > IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
  > FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
  > COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
  > IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  > CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
  > ```
  """

  alias Purl.Error.DuplicateQualifier
  alias Purl.Error.InvalidField
  alias Purl.Error.InvalidScheme
  alias Purl.Error.TypeValidationFailed

  require Record

  Record.defrecordp(:purl, Record.extract(:purl, from: "include/purl.hrl"))

  # credo:disable-for-next-line Credo.Check.Warning.SpecWithStruct
  @type parse_error ::
          %URI.Error{}
          | InvalidField.t()
          | DuplicateQualifier.t()
          | InvalidScheme.t()
          | TypeValidationFailed.t()

  @typedoc """
  Field types that can be validated in PURL type specifications
  """
  @type validation_field :: :purl.validation_field()

  @typedoc """
  the package "type" or package "protocol" such as `maven`, `npm`, `nuget`,
  `gem`, `pypi`, etc.

  Known types: https://github.com/package-url/purl-spec/blob/master/PURL-TYPES.rst

  ## Validation

  * The package type is composed only of ASCII letters and numbers, '.', '+' and '-' (period, plus, and dash)
  * The type cannot start with a number
  * The type cannot contains spaces
  * The type must NOT be percent-encoded
  * The type is case insensitive. The canonical form is lowercase
  """
  @type type :: :purl.type()

  @typedoc """
  Segment of the namespace

  ## Validation

  * must not contain a '/'
  * must not be empty
  * A URL host or Authority must NOT be used as a namespace. Use instead a
  `repository_url` qualifier. Note however that for some types, the namespace
  may look like a host.
  """
  @type namespace_segment :: :purl.namespace_segment()

  @typedoc """
  some name prefix such as a Maven groupid, a Docker image owner, a GitHub user
  or organization

  The values are type-specific.
  """
  @type namespace :: :purl.namespace()

  @typedoc """
  the name of the package
  """
  @type name :: :purl.name()

  @typedoc """
  the version of the package

  A version is a plain and opaque string. Some package types use versioning
  conventions such as semver for NPMs or nevra conventions for RPMS. A type may
  define a procedure to compare and sort versions, but there is no reliable and
  uniform way to do such comparison consistently.
  """
  @type version :: :purl.version() | Version.t()

  @typedoc """
  qualifier key

  ## Validation

  * The key must be composed only of ASCII letters and numbers, '.', '-' and '_' (period, dash and underscore)
  * A key cannot start with a number
  * A key must NOT be percent-encoded
  * A key is case insensitive. The canonical form is lowercase
  * A key cannot contains spaces
  """
  @type qualifier_key :: :purl.qualifier_key()

  @typedoc """
  qualifier value

  ## Validation
  * value cannot be an empty string: a key=value pair with an empty value is the
  same as no key/value at all for this key
  """
  @type qualifier_value :: :purl.qualifier_value()

  @typedoc """
  extra qualifying data for a package such as an OS, architecture, a distro,
  etc.

  The values are type-specific.

  ## Validation
  * key must be unique within the keys of the qualifiers string
  """
  @type qualifiers :: :purl.qualifiers()

  @typedoc """
  subpath segment

  ## Validation
  * must not contain a '/'
  * must not be any of '..' or '.'
  * must not be empty
  """
  @type subpath_segment :: :purl.subpath_segment()

  @typedoc """
  extra subpath within a package, relative to the package root
  """
  @type subpath :: :purl.subpath()

  @typedoc """
  Package URL struct
  """
  @type t :: %__MODULE__{
          type: type(),
          namespace: namespace(),
          name: name(),
          version: version() | nil,
          qualifiers: qualifiers(),
          subpath: subpath()
        }

  @enforce_keys [:type, :name]
  defstruct [:type, :name, namespace: [], version: nil, qualifiers: %{}, subpath: []]

  @doc """
  Formats purl as string

  ## Examples

      iex> Purl.to_string(%Purl{type: "hex", name: "purl"})
      "pkg:hex/purl"

  """
  @spec to_string(purl :: t()) :: String.t()
  def to_string(%Purl{} = purl), do: purl |> Purl.to_record() |> :purl.to_binary()

  @doc """
  Converts a purl to a `URI`

  ## Examples

      iex> Purl.to_uri(%Purl{type: "hex", name: "purl"})
      %URI{scheme: "pkg", path: "hex/purl"}

  """
  @spec to_uri(purl :: t()) :: URI.t()
  def to_uri(%Purl{} = purl), do: purl |> Purl.to_record() |> :purl.to_uri() |> then(&struct!(URI, &1))

  @doc """
  Creates a new purl struct from a `Purl`, `URI` or string.

  ## Examples

      iex> Purl.new("pkg:hex/purl")
      {:ok, %Purl{type: "hex", name: "purl"}}

  """
  @spec new(purl :: String.t() | URI.t() | t()) :: {:ok, t()} | {:error, parse_error()}
  def new(purl)
  def new(%Purl{} = purl), do: purl |> to_record() |> :purl.new() |> purl_response()
  def new(purl), do: purl |> :purl.new() |> purl_response()

  @doc """
  Similar to `new/1` but raises `URI.Error`, `Purl.Error.InvalidField` or
  `Purl.Error.InvalidURI` if an invalid input is given.

  ## Examples

      iex> Purl.new!("pkg:hex/purl")
      %Purl{type: "hex", name: "purl"}

      iex> Purl.new!(">pkg:hex/purl")
      ** (URI.Error) cannot parse due to reason invalid_uri: ">"

      iex> Purl.new!("pkg:hex*/purl")
      ** (Purl.Error.InvalidField) invalid field type, \"hex*\" given

  """
  @spec new!(purl :: String.t() | URI.t() | t()) :: t()
  def new!(purl) do
    case new(purl) do
      {:ok, purl} -> purl
      {:error, reason} -> raise reason
    end
  end

  @doc """
  Convert known URLs to purl

  ## Currently Supported

  * GitHub: Repository HTTP / Git URL, Project URL
  * BitBucket: Repository HTTTP / Git URL, Project URL
  * Hex.pm package URL

  """
  @spec from_resource_uri(uri :: String.t() | URI.t(), fallback_version :: Version.t() | String.t() | nil) ::
          {:ok, t()} | :error
  def from_resource_uri(uri, fallback_version \\ nil), do: _from_resource_uri(uri, fallback_version)

  @spec _from_resource_uri(uri :: String.t() | URI.t(), fallback_version :: Version.t() | String.t() | nil | :undefined) ::
          {:ok, t()} | :error
  defp _from_resource_uri(uri, %Version{} = version), do: _from_resource_uri(uri, Version.to_string(version))
  defp _from_resource_uri(uri, nil), do: _from_resource_uri(uri, :undefined)
  defp _from_resource_uri(%URI{} = uri, fallback_version), do: _from_resource_uri(URI.to_string(uri), fallback_version)
  defp _from_resource_uri(uri, fallback_version), do: uri |> :purl.from_resource_uri(fallback_version) |> purl_response()

  @doc false
  @spec to_record(purl :: t()) :: :purl.t()
  def to_record(%Purl{} = purl) do
    version =
      case purl.version do
        nil -> :undefined
        %Version{} = version -> Version.to_string(version)
        version -> version
      end

    purl(
      type: purl.type,
      name: purl.name,
      namespace: purl.namespace,
      version: version,
      qualifiers: purl.qualifiers,
      subpath: purl.subpath
    )
  end

  @doc false
  @spec from_record(:purl.t()) :: t()
  def from_record(purl) do
    version =
      case purl(purl, :version) do
        :undefined -> nil
        version -> version
      end

    %Purl{
      type: purl(purl, :type),
      name: purl(purl, :name),
      namespace: purl(purl, :namespace),
      version: version,
      qualifiers: purl(purl, :qualifiers),
      subpath: purl(purl, :subpath)
    }
  end

  @doc false
  @spec purl_response(response :: {:ok, :purl.t()} | :purl.parse_error() | :error) ::
          {:ok, t()} | {:error, parse_error()} | :error
  defp purl_response(response)
  defp purl_response({:ok, purl}), do: {:ok, from_record(purl)}

  defp purl_response({:error, {:invalid_field, field, value}}),
    do: {:error, InvalidField.exception(field: field, value: value)}

  defp purl_response({:error, {:invalid_scheme, scheme}}), do: {:error, InvalidScheme.exception(scheme: scheme)}
  defp purl_response({:error, {:duplicate_qualifier, key}}), do: {:error, DuplicateQualifier.exception(key: key)}

  defp purl_response({:error, {:type_validation, type, field, value, reason}}),
    do: {:error, TypeValidationFailed.exception(type: type, field: field, value: value, reason: reason)}

  defp purl_response({:error, reason, term}),
    do: {:error, %URI.Error{action: :parse, reason: reason, part: Kernel.to_string(term)}}

  defp purl_response(:error), do: :error

  defimpl String.Chars do
    @impl String.Chars
    def to_string(%Purl{} = purl), do: Purl.to_string(purl)
  end

  defimpl Inspect do
    import Inspect.Algebra

    @impl Inspect
    def inspect(%Purl{} = purl, opts) do
      concat(["Purl.new!(", to_doc(Purl.to_string(purl), opts), ")"])
    end
  end
end
