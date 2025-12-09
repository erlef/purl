defmodule PurlTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  doctest Purl

  describe inspect(&Purl.new/1) do
    test "should return a purl for a string" do
      assert {:ok,
              %Purl{
                type: "hex",
                namespace: ["name", "space"],
                name: "purl",
                version: "1.0.0",
                qualifiers: %{"key" => "value"},
                subpath: ["path", "to", "directory"]
              }} = Purl.new("pkg:hex/name/space/purl@1.0.0?key=value#/path/to/directory/")
    end

    test "throws error on duplicate qualifiers" do
      assert_raise Purl.Error.DuplicateQualifier, fn ->
        Purl.new!("pkg:hex/purl?foo=bar&foo=bar")
      end
    end

    test "throws error on invalid schema" do
      assert %Purl{} = Purl.new!("pkg:hex/purl")
      assert %Purl{} = Purl.new!("PKG:hex/purl")

      assert_raise Purl.Error.InvalidScheme, fn ->
        Purl.new!("pkgs:hex/purl")
      end
    end

    test "fixes url with protocol" do
      assert Purl.new!("pkg:hex/purl") == "pkg://hex/purl" |> Purl.new!() |> Purl.new!()
    end

    test "ignores broken mlflow url" do
      assert %Purl{} = Purl.new!("pkg:mlflow/CreditFraud@3?repository_url=not_a_url")
    end

    test "failed special case" do
      assert_raise Purl.Error.TypeValidationFailed,
                   "Type validation failed for field `namespace` with value `` in PURL of type `swift`: Namespace is required but missing.",
                   fn ->
                     Purl.new!("pkg:swift/foo")
                   end
    end
  end

  describe inspect(&Purl.from_resource_uri/1) do
    test "GitHub git url" do
      assert {:ok,
              %Purl{
                type: "github",
                namespace: ["jshmrtn"],
                name: "purl",
                qualifiers: %{
                  "vcs_url" => "git+https://github.com/jshmrtn/purl.git",
                  "download_url" => "https://github.com/jshmrtn/purl/archive/HEAD.tar.gz"
                }
              }} =
               Purl.from_resource_uri("git@github.com:jshmrtn/purl.git")

      assert {:ok,
              %Purl{
                type: "github",
                namespace: ["jshmrtn"],
                name: "purl",
                version: "v1.2.3",
                qualifiers: %{
                  "vcs_url" => "git+https://github.com/jshmrtn/purl.git",
                  "download_url" => "https://github.com/jshmrtn/purl/archive/v1.2.3.tar.gz"
                }
              }} =
               Purl.from_resource_uri("git@github.com:jshmrtn/purl.git", "v1.2.3")

      assert :error = Purl.from_resource_uri("git@github.com:jshmrtn")
    end

    test "GitHub http url" do
      assert {:ok,
              %Purl{
                type: "github",
                namespace: ["jshmrtn"],
                name: "purl",
                version: "HEAD",
                qualifiers: %{
                  "vcs_url" => "git+https://github.com/jshmrtn/purl.git",
                  "download_url" => "https://github.com/jshmrtn/purl/archive/HEAD.tar.gz"
                }
              }} =
               "https://github.com/jshmrtn/purl.git" |> URI.new!() |> Purl.from_resource_uri()

      assert {:ok,
              %Purl{
                type: "github",
                namespace: ["jshmrtn"],
                name: "purl",
                version: "main",
                qualifiers: %{
                  "vcs_url" => "git+https://github.com/jshmrtn/purl.git",
                  "download_url" => "https://github.com/jshmrtn/purl/archive/main.tar.gz"
                }
              }} =
               "https://github.com/jshmrtn/purl/tree/main/.github" |> URI.new!() |> Purl.from_resource_uri()

      assert {:ok,
              %Purl{
                type: "github",
                namespace: ["jshmrtn"],
                name: "purl",
                version: "v1.2.3",
                qualifiers: %{
                  "vcs_url" => "git+https://github.com/jshmrtn/purl.git",
                  "download_url" => "https://github.com/jshmrtn/purl/archive/v1.2.3.tar.gz"
                }
              }} =
               "https://github.com/jshmrtn/purl/releases/tag/v1.2.3" |> URI.new!() |> Purl.from_resource_uri()

      assert {:ok,
              %Purl{
                type: "github",
                namespace: ["jshmrtn"],
                name: "purl",
                version: "v1.2.3",
                qualifiers: %{
                  "vcs_url" => "git+https://github.com/jshmrtn/purl.git",
                  "download_url" => "https://github.com/jshmrtn/purl/archive/v1.2.3.tar.gz"
                }
              }} =
               "https://github.com/jshmrtn/purl.git" |> URI.new!() |> Purl.from_resource_uri("v1.2.3")

      assert :error = "https://github.com/jshmrtn" |> URI.new!() |> Purl.from_resource_uri()
    end

    test "BitBucket git url" do
      assert {:ok,
              %Purl{
                type: "bitbucket",
                namespace: ["jshmrtn"],
                name: "purl",
                qualifiers: %{
                  "vcs_url" => "git+https://bitbucket.org/jshmrtn/purl.git",
                  "download_url" => "https://bitbucket.org/jshmrtn/purl/get/HEAD.tar.gz"
                }
              }} =
               Purl.from_resource_uri("git@bitbucket.org:jshmrtn/purl.git")

      assert {:ok,
              %Purl{
                type: "bitbucket",
                namespace: ["jshmrtn"],
                name: "purl",
                version: "v1.2.3",
                qualifiers: %{
                  "vcs_url" => "git+https://bitbucket.org/jshmrtn/purl.git",
                  "download_url" => "https://bitbucket.org/jshmrtn/purl/get/v1.2.3.tar.gz"
                }
              }} =
               Purl.from_resource_uri("git@bitbucket.org:jshmrtn/purl.git", "v1.2.3")

      assert :error = Purl.from_resource_uri("git@bitbucket.org:jshmrtn")
    end

    test "BitBucket http url" do
      assert {:ok,
              %Purl{
                type: "bitbucket",
                namespace: ["jshmrtn"],
                name: "purl",
                qualifiers: %{
                  "vcs_url" => "git+https://bitbucket.org/jshmrtn/purl.git",
                  "download_url" => "https://bitbucket.org/jshmrtn/purl/get/HEAD.tar.gz"
                }
              }} =
               Purl.from_resource_uri("https://irrelevant@bitbucket.org/jshmrtn/purl.git")

      assert {:ok,
              %Purl{
                type: "bitbucket",
                namespace: ["jshmrtn"],
                name: "purl",
                version: "v1.2.3",
                qualifiers: %{
                  "vcs_url" => "git+https://bitbucket.org/jshmrtn/purl.git",
                  "download_url" => "https://bitbucket.org/jshmrtn/purl/get/v1.2.3.tar.gz"
                }
              }} =
               Purl.from_resource_uri("https://irrelevant@bitbucket.org/jshmrtn/purl.git", "v1.2.3")

      assert :error = Purl.from_resource_uri("https://irrelevant@bitbucket.org/jshmrtn")
    end

    test "Hex.pm http url" do
      assert {:ok, %Purl{type: "hex", namespace: [], name: "expo"}} =
               Purl.from_resource_uri("https://hex.pm/packages/expo")

      assert {:ok,
              %Purl{
                type: "hex",
                namespace: [],
                name: "expo",
                version: "1.0.0",
                qualifiers: %{"download_url" => "https://repo.hex.pm/tarballs/expo/1.0.0.tar"}
              }} =
               Purl.from_resource_uri("https://hex.pm/packages/expo/1.0.0")

      assert {:ok, %Purl{type: "hex", namespace: [], name: "expo", version: nil}} =
               Purl.from_resource_uri("https://hex.pm/packages/expo/versions")
    end

    test "Random URL" do
      assert :error = Purl.from_resource_uri("https://example.com")
      assert :error = Purl.from_resource_uri("git@example\0com")
    end
  end

  describe inspect(&Prul.to_string/1) do
    test "should encode qualifiers correctly" do
      purl = %Purl{type: "hex", name: "purl", qualifiers: %{"key" => "value&other=value"}}
      assert purl |> Purl.to_string() |> Purl.new!() == purl
    end
  end

  describe "fuzzer" do
    @tag timeout: 5 * 60 * 1_000
    property "compose / parse gives same result" do
      check all(purl <- Purl.Generator.purl()) do
        # Uppercase fields should still parse, but the canonical form is lowercase
        comparison = %{
          purl
          | type: String.downcase(purl.type),
            qualifiers: Map.new(purl.qualifiers, fn {key, value} -> {String.downcase(key), value} end),
            version:
              case purl.version do
                %Version{} = version -> Version.to_string(version)
                string when is_binary(string) -> string
                nil -> nil
              end
        }

        canonical = Purl.to_string(purl)
        to_string = to_string(purl)
        inspected = inspect(purl, printable_limit: 1_000)

        assert {:ok, comparison} == Purl.new(canonical)
        assert {:ok, comparison} == Purl.new(to_string)

        if String.length(canonical) <= 1000 do
          assert {comparison, []} == Code.eval_string(inspected)
        end
      end
    end
  end
end
