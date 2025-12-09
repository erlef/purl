# credo:disable-for-this-file Credo.Check.Refactor.Nesting
with {:module, StreamData} <- Code.ensure_loaded(StreamData) do
  defmodule Purl.Generator do
    @moduledoc """
    `StreamData` generator for valid purls
    """

    import StreamData

    alias Purl.Generator.Version, as: VersionGenerator

    @type generator_opts :: [lowercase: boolean()]

    @spec type :: StreamData.t(Purl.type())
    def type do
      unknown_types =
        [?a..?z, ?A..?Z, ?0..?9, ?., ?+, ?-]
        |> string(min_length: 1, max_length: 20)
        |> require_letter_start()

      known_types =
        :purl_type_registry.list_types()
        |> Enum.map(&constant/1)
        |> one_of()

      one_of([known_types, unknown_types])
    end

    @spec namespace_segment(generator_opts()) :: StreamData.t(Purl.namespace_segment())
    def namespace_segment(opts \\ []) do
      generator = filter(string(:printable, min_length: 1, max_length: 50), &(not String.contains?(&1, "/")))

      if Keyword.get(opts, :lowercase, false) do
        map(generator, &String.downcase/1)
      else
        generator
      end
    end

    @spec namespace(generator_opts()) :: StreamData.t(Purl.namespace())
    def namespace(opts \\ []), do: list_of(namespace_segment(opts), max_length: 10)

    @spec name(generator_opts()) :: StreamData.t(Purl.name())
    def name(opts \\ []) do
      generator = string(:printable, min_length: 1, max_length: 50)

      if Keyword.get(opts, :lowercase, false) do
        map(generator, &String.downcase/1)
      else
        generator
      end
    end

    @spec version(generator_opts()) :: StreamData.t(Purl.version())
    def version(opts \\ []) do
      string_gen = string(:printable, max_length: 50)

      string_gen =
        if Keyword.get(opts, :lowercase, false) do
          map(string_gen, &String.downcase/1)
        else
          string_gen
        end

      one_of([string_gen, VersionGenerator.version(opts)])
    end

    @spec qualifier_key :: StreamData.t(Purl.qualifier_key())
    def qualifier_key,
      do: require_letter_start(string([?a..?z, ?A..?Z, ?0..?9, ?., ?-, ?_], min_length: 1, max_length: 50))

    @spec qualifier_value :: StreamData.t(Purl.qualifier_value())
    def qualifier_value, do: string(:printable, min_length: 1, max_length: 50)

    @spec qualifiers :: StreamData.t(Purl.qualifiers())
    def qualifiers,
      do: discard_case_insensitive_duplicate_map_keys(map_of(qualifier_key(), qualifier_value(), max_length: 15))

    @spec subpath_segment(generator_opts()) :: StreamData.t(Purl.subpath_segment())
    def subpath_segment(opts \\ []) do
      generator =
        filter(string(:printable, min_length: 1, max_length: 50), fn
          "." -> false
          ".." -> false
          subpath -> not String.contains?(subpath, "/")
        end)

      if Keyword.get(opts, :lowercase, false) do
        map(generator, &String.downcase/1)
      else
        generator
      end
    end

    @spec subpath(generator_opts()) :: StreamData.t(Purl.subpath())
    def subpath(opts \\ []), do: list_of(subpath_segment(opts), max_length: 10)

    @spec purl :: StreamData.t(Purl.t())
    def purl do
      bind(type(), fn type ->
        spec = :purl_type_registry.lookup(type)

        namespace_gen = namespace(lowercase: not spec.namespace_definition.case_sensitive)
        name_gen = name(lowercase: not spec.name_definition.case_sensitive)
        version_gen = version(lowercase: not spec.version_definition.case_sensitive)
        subpath_gen = subpath(lowercase: not spec.subpath_definition.case_sensitive)

        bind(generate_conditional_list(namespace_gen, spec.namespace_definition.requirement), fn namespace ->
          bind(generate_by_requirement(name_gen, spec.name_definition.requirement), fn name ->
            bind(generate_by_requirement(version_gen, spec.version_definition.requirement), fn version ->
              bind(generate_spec_qualifiers(spec.qualifiers_definition), fn qualifiers ->
                bind(generate_conditional_list(subpath_gen, spec.subpath_definition.requirement), fn subpath ->
                  constant(%Purl{
                    type: type,
                    namespace: namespace || [],
                    name: name,
                    version: version,
                    qualifiers: qualifiers,
                    subpath: subpath || []
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

    @spec generate_by_requirement(StreamData.t(term()), String.t()) :: StreamData.t(term() | nil)
    defp generate_by_requirement(_generator, "prohibited"), do: constant(nil)
    defp generate_by_requirement(generator, "required"), do: generator
    defp generate_by_requirement(generator, "optional"), do: one_of([constant(nil), generator])

    @spec generate_conditional_list(StreamData.t(term()), String.t()) :: StreamData.t(list())
    defp generate_conditional_list(_generator, "prohibited"), do: constant([])
    defp generate_conditional_list(generator, "required"), do: filter(generator, &(length(&1) > 0))
    defp generate_conditional_list(generator, "optional"), do: one_of([constant([]), generator])

    @spec generate_spec_qualifiers(list()) :: StreamData.t(map())
    defp generate_spec_qualifiers(qualifiers_def) do
      required_qualifiers =
        qualifiers_def
        |> Enum.filter(&(&1[:requirement] == "required"))
        |> Enum.map(&{&1.key, qualifier_value()})

      optional_qualifiers =
        qualifiers_def
        |> Enum.filter(&(Map.get(&1, :requirement, "optional") == "optional"))
        |> Enum.map(&{&1.key, qualifier_value()})

      required_map =
        if required_qualifiers == [] do
          constant(%{})
        else
          fixed_map(required_qualifiers)
        end

      optional_map =
        if optional_qualifiers == [] do
          constant(%{})
        else
          one_of([constant(%{}), fixed_map(optional_qualifiers)])
        end

      # Combine required and optional qualifiers, plus allow arbitrary additional ones
      bind(required_map, fn req_quals ->
        bind(optional_map, fn opt_quals ->
          bind(qualifiers(), fn extra_quals ->
            # Filter out any extra qualifiers that conflict with spec-defined ones
            spec_keys = Enum.map(qualifiers_def, & &1.key)
            filtered_extra = Map.drop(extra_quals, spec_keys)

            req_quals
            |> Map.merge(opt_quals)
            |> Map.merge(filtered_extra)
            |> constant()
          end)
        end)
      end)
    end
  end
end
