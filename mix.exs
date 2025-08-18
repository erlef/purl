# credo:disable-for-this-file Credo.Check.Readability.Specs
defmodule Purl.MixProject do
  use Mix.Project

  {:ok, [{:application, :purl, props}]} = :file.consult(~c"src/purl.app.src")
  @props Keyword.take(props, [:applications, :description, :env, :mod, :licenses, :vsn])

  @source_url "https://github.com/erlef/purl"

  def project do
    [
      app: :purl,
      version: to_string(@props[:vsn]),
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: docs(),
      test_coverage: [tool: ExCoveralls],
      description: to_string(@props[:description]),
      dialyzer:
        [list_unused_filters: true] ++
          if (System.get_env("DIALYZER_PLT_PRIV") || "false") in ["1", "true"] do
            [plt_file: {:no_warn, "priv/plts/dialyzer.plt"}]
          else
            []
          end,
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.detail": :test,
        "coveralls.html": :test,
        "coveralls.json": :test,
        "coveralls.post": :test,
        "coveralls.xml": :test
      ],
      package: package()
    ]
  end

  defp package do
    %{
      licenses: Enum.map(@props[:licenses], &to_string/1),
      maintainers: ["Jonatan Männchen"],
      links: %{
        "GitHub" => @source_url,
        "Changelog" => @source_url <> "/releases",
        "Issues" => @source_url <> "/issues",
        "purl Specification" => "https://github.com/package-url/purl-spec"
      },
      build_tools: ["rebar3", "mix"],
      files: [
        "include",
        "lib",
        "LICENSE*",
        "mix.exs",
        "README*",
        "rebar.config",
        "src"
      ]
    }
  end

  def application do
    []
  end

  defp docs do
    [
      source_url: @source_url,
      source_ref: "v" <> to_string(@props[:vsn]),
      main: "readme",
      extras: ["README.md"],
      groups_for_modules: [Erlang: [~r/purl/], "Elixir": [~r/^Purl/]]
    ]
  end

  defp deps do
    [
      {:credo, "~> 1.0", only: [:dev], runtime: false},
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:excoveralls, "~> 0.5", only: [:test], runtime: false},
      {:ex_doc, ">= 0.0.0", only: [:dev], runtime: false},
      {:jason, "~> 1.4", only: [:dev, :test]},
      {:stream_data, "~> 1.1", optional: true},
      {:styler, "~> 1.4", only: [:dev, :test], runtime: false}
    ]
  end
end
