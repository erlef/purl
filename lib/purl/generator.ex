# credo:disable-for-this-file Credo.Check.Refactor.Nesting
with {:module, StreamData} <- Code.ensure_loaded(StreamData) do
  defmodule Purl.Generator do
    @moduledoc """
    `StreamData` generator for valid purls
    """

    import StreamData

    alias Purl.Generator.Version, as: VersionGenerator

    @spec type :: StreamData.t(Purl.type())
    def type do
      [?a..?z, ?A..?Z, ?0..?9, ?., ?+, ?-]
      |> string(min_length: 1, max_length: 20)
      |> require_letter_start()
      # Discard Known Types since those have restrictions on segments of the purl
      |> filter(fn type ->
        case :purl_type_registry.lookup(type) do
          %{"$id": "unregistered://" <> _rest} -> true
          _known_type -> false
        end
      end)
    end

    @spec namespace_segment :: StreamData.t(Purl.namespace_segment())
    def namespace_segment, do: filter(string(:printable, min_length: 1, max_length: 50), &(not String.contains?(&1, "/")))

    @spec namespace :: StreamData.t(Purl.namespace())
    def namespace, do: list_of(namespace_segment(), max_length: 10)

    @spec name :: StreamData.t(Purl.name())
    def name, do: string(:printable, min_length: 1, max_length: 50)

    @spec version :: StreamData.t(Purl.version())
    def version, do: one_of([string(:printable, max_length: 50), VersionGenerator.version()])

    @spec qualifier_key :: StreamData.t(Purl.qualifier_key())
    def qualifier_key,
      do: require_letter_start(string([?a..?z, ?A..?Z, ?0..?9, ?., ?-, ?_], min_length: 1, max_length: 50))

    @spec qualifier_value :: StreamData.t(Purl.qualifier_value())
    def qualifier_value, do: string(:printable, min_length: 1, max_length: 50)

    @spec qualifiers :: StreamData.t(Purl.qualifiers())
    def qualifiers,
      do: discard_case_insensitive_duplicate_map_keys(map_of(qualifier_key(), qualifier_value(), max_length: 15))

    @spec subpath_segment :: StreamData.t(Purl.subpath_segment())
    def subpath_segment do
      filter(string(:printable, min_length: 1, max_length: 50), fn
        "." -> false
        ".." -> false
        subpath -> not String.contains?(subpath, "/")
      end)
    end

    @spec subpath :: StreamData.t(Purl.subpath())
    def subpath, do: list_of(subpath_segment(), max_length: 10)

    @spec purl :: StreamData.t(Purl.t())
    def purl do
      bind(type(), fn type ->
        bind(namespace(), fn namespace ->
          bind(name(), fn name ->
            bind(version(), fn version ->
              bind(qualifiers(), fn qualifiers ->
                bind(subpath(), fn subpath ->
                  constant(%Purl{
                    type: type,
                    namespace: namespace,
                    name: name,
                    version: version,
                    qualifiers: qualifiers,
                    subpath: subpath
                  })
                end)
              end)
            end)
          end)
        end)
      end)
    end

    @spec require_letter_start(generator :: StreamData.t(inner)) :: StreamData.t(inner) when inner: String.t()
    defp require_letter_start(generator) do
      filter(generator, fn
        <<letter::utf8>> <> _rest when letter in ?a..?z or letter in ?A..?Z -> true
        _value -> false
      end)
    end

    @spec discard_case_insensitive_duplicate_map_keys(generator :: StreamData.t(inner)) :: StreamData.t(inner)
          when inner: %{String.t() => term()}
    defp discard_case_insensitive_duplicate_map_keys(generator) do
      filter(generator, fn map ->
        map |> Map.keys() |> Enum.map(&String.downcase/1) |> Enum.uniq() |> Enum.count() == map_size(map)
      end)
    end
  end
end
