# TODO: Report issues and fix excluded tests
excluded_tests = [
  # https://github.com/package-url/purl-spec/blob/fdbdf5d84940e6a303e364decb0df1f4c558ec0d/tests/types/maven-test.json#L569-L584
  # There's no colon in the test...?
  {"maven-test", "build", "invalid encoded colon : between scheme and type"},

  # Case Sensitivity described in text, but not specified
  {"mlflow-test", "parse", "MLflow model tracked in Azure Databricks (case insensitive)"},
  {"mlflow-test", "roundtrip",
   "MLflow model tracked in Azure Databricks (case insensitive). Rountrip an input purl to canonical."},

  # Case Sensitive test contradicts the specification
  {"cpan-test", "parse", "Parse test for <class 'type'> PURL"},
  {"cpan-test", "build", "cpan distribution name like module name"},
  {"cpan-test", "build", "cpan module name like distribution name"},
  {"cpan-test", "parse", "cpan module name like distribution name"},
  {"cpan-test", "build", "Build test  for <class 'type'> PURL"},
  {"cpan-test", "parse", "Should fail to parse a PURL from invalid canonical purl input"},
  {"cpan-test", "parse", "cpan distribution name like module name"},

  # Requires Version, but that can't be defined in the specification
  {"swift-test", "parse", "invalid swift purl without version"},
  {"swift-test", "build", "invalid swift purl without version"},

  # Requires Version but none were provided in test
  {"huggingface-test", "build", "Build test  for <class 'type'> PURL"},
  {"huggingface-test", "parse", "Parse test for <class 'type'> PURL"},
  {"huggingface-test", "roundtrip", "Rountrip test for <class 'type'> PURL"},
  {"huggingface-test", "build", "minimal Hugging Face model"},
  {"huggingface-test", "roundtrip", "minimal Hugging Face model. Rountrip a canonical input to canonical output."},
  {"huggingface-test", "roundtrip", "minimal Hugging Face model. Rountrip an input purl to canonical."},
  {"huggingface-test", "parse", "minimal Hugging Face model"},
  {"rpm-test", "parse", "Parse test for <class 'type'> PURL"},
  {"rpm-test", "build", "Build test  for <class 'type'> PURL"},
  {"rpm-test", "roundtrip", "Rountrip test for <class 'type'> PURL"},

  # Conan ???
  {"conan-test", "parse", "invalid conan purl only channel qualifier"},
  {"conan-test", "build", "invalid conan purl only channel qualifier"},
  {"conan-test", "build", "invalid conan purl only namespace"},
  {"conan-test", "parse", "invalid conan purl only namespace"},

  # Cran always has a version, but the specification does not require it
  # Additionally, why would I not be able to point to a package when not knowing
  # the version?
  {"cran-test", "parse", "invalid cran purl without version"},
  {"cran-test", "build", "invalid cran purl without version"}
]

parameters =
  for file <- :purl |> Application.app_dir("priv/spec/tests/**/*-test.json") |> Path.wildcard(),
      test_file_name = Path.basename(file, ".json"),
      # credo:disable-for-next-line Credo.Check.Warning.UnsafeToAtom
      test_suite = file |> File.read!() |> Jason.decode!(keys: :atoms),
      %{"$schema": "https://packageurl.org/schemas/purl-test.schema-1.0.json", tests: tests} = test_suite,
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

  test("run specification test", %{spec_test: spec_test}, do: run_test(spec_test))

  @spec run_test(test :: map()) :: :ok
  defp run_test(test)

  defp run_test(%{test_type: "parse", expected_failure: true, input: input}) do
    assert {:error, _reason} = Purl.new(input)
  end

  defp run_test(%{test_type: "parse", expected_failure: false, input: input, expected_output: expected_output}) do
    assert {:ok, purl} = Purl.new(input)

    assert purl == test_components_to_purl(expected_output)
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
