# TODO: Report issues and fix excluded tests
excluded_tests = [
  # Case Sensitivity described in text, but not specified
  {"mlflow-test", "parse", "MLflow model tracked in Azure Databricks (case insensitive)"},
  {"mlflow-test", "roundtrip",
   "MLflow model tracked in Azure Databricks (case insensitive). Roundtrip an input purl to canonical."},

  # Case Sensitive test contradicts the specification
  # https://github.com/package-url/purl-spec/pull/634
  {"cpan-test", "build", "CPAN distribution name like module name"},
  {"cpan-test", "parse", "CPAN distribution name as module name"}
]

parameters =
  for file <- :purl |> Application.app_dir("priv/spec/tests/**/*-test.json") |> Path.wildcard(),
      test_file_name = Path.basename(file, ".json"),
      # credo:disable-for-next-line Credo.Check.Warning.UnsafeToAtom
      test_suite = file |> File.read!() |> Jason.decode!(keys: :atoms),
      %{"$schema": schema, tests: tests} = test_suite,
      true =
        match?(
          schema
          when schema in [
                 "https://packageurl.org/schemas/purl-test.schema-0.1.json",
                 "https://packageurl.org/schemas/purl-test.schema-1.0.json"
               ],
          schema
        ),
      %{description: test_description, test_type: test_type} = test <- tests,
      {test_file_name, test_type, test_description} not in excluded_tests do
    %{
      spec_test_file: Path.relative_to_cwd(file),
      spec_test_name: test_file_name,
      spec_test: test
    }
  end

defmodule PurlSpecificationComplianceTest do
  use ExUnit.Case,
    async: true,
    parameterize: parameters

  if Version.match?(System.version(), "<= 1.18.0") do
    @moduletag :skip
  end

  test("run specification test", %{spec_test: spec_test}, do: run_test(spec_test))

  @spec run_test(test :: map()) :: :ok
  defp run_test(test)

  defp run_test(%{test_type: "parse", expected_failure: true, input: input}) do
    assert {:error, _reason} = Purl.new(input)
  end

  defp run_test(%{test_type: "parse", expected_failure: false, input: input, expected_output: expected_output}) do
    assert {:ok, purl} = Purl.new(input)

    assert Purl.equal?(purl, test_components_to_purl(expected_output))
  end

  defp run_test(%{test_type: "build", expected_failure: true, input: %{type: nil}}) do
    # Skip test, this is not a valid case
  end

  defp run_test(%{test_type: "build", expected_failure: true, input: input}) do
    result =
      with {:ok, purl} <- input |> test_components_to_purl() |> Purl.new(),
           do: {:ok, Purl.to_string(purl)}

    assert {:error, _reason} = result
  end

  defp run_test(%{test_type: "build", expected_failure: false, input: input, expected_output: expected_output}) do
    assert {:ok, purl} = input |> test_components_to_purl() |> Purl.new()
    assert Purl.to_string(purl) == expected_output
  end

  defp run_test(%{test_type: "roundtrip", input: input, expected_output: expected_output}) do
    assert {:ok, purl} = Purl.new(input)
    assert Purl.to_string(purl) == expected_output
  end

  defp run_test(test), do: raise("Unsupported test type: #{test.test_type}")

  @spec test_components_to_purl(components :: map()) :: Purl.t()
  defp test_components_to_purl(components) do
    %Purl{
      type: components.type,
      namespace:
        case components.namespace do
          nil -> []
          namespace -> String.split(namespace, "/")
        end,
      name: components.name,
      version: components.version,
      qualifiers:
        Map.new(components.qualifiers || %{}, fn {k, v} ->
          {Atom.to_string(k), v}
        end),
      subpath:
        case components.subpath do
          nil -> []
          subpath -> String.split(subpath, "/")
        end
    }
  end
end
