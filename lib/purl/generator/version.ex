# credo:disable-for-this-file Credo.Check.Refactor.Nesting
with {:module, StreamData} <- Code.ensure_loaded(StreamData) do
  defmodule Purl.Generator.Version do
    @moduledoc false

    import StreamData

    @type generator_opts :: [lowercase: boolean()]

    @spec major :: StreamData.t(Version.major())
    defp major, do: non_negative_integer()

    @spec minor :: StreamData.t(Version.minor())
    defp minor, do: non_negative_integer()

    @spec patch :: StreamData.t(Version.patch())
    defp patch, do: non_negative_integer()

    @spec pre(generator_opts()) :: StreamData.t(Version.pre())
    defp pre(opts) do
      string_gen = string(:printable, max_length: 20)

      string_gen =
        if Keyword.get(opts, :lowercase, false) do
          map(string_gen, &String.downcase/1)
        else
          string_gen
        end

      list_of(one_of([non_negative_integer(), string_gen]), max_length: 5)
    end

    @spec build(generator_opts()) :: StreamData.t(Version.build())
    defp build(opts) do
      string_gen = string(:printable, max_length: 20)

      string_gen =
        if Keyword.get(opts, :lowercase, false) do
          map(string_gen, &String.downcase/1)
        else
          string_gen
        end

      one_of([constant(nil), string_gen])
    end

    @spec version(generator_opts()) :: StreamData.t(Version.t())
    def version(opts \\ []) do
      bind(major(), fn major ->
        bind(minor(), fn minor ->
          bind(patch(), fn patch ->
            bind(pre(opts), fn pre ->
              bind(build(opts), fn build ->
                constant(%Version{
                  build: build,
                  major: major,
                  minor: minor,
                  patch: patch,
                  pre: pre
                })
              end)
            end)
          end)
        end)
      end)
    end
  end
end
