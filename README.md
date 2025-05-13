# Purl

[![EEF Security WG project](https://img.shields.io/badge/EEF-Security-black)](https://github.com/erlef/security-wg)
[![hex.pm badge](https://img.shields.io/badge/Package%20on%20hex.pm-informational)](https://hex.pm/packages/purl)
[![Documentation badge](https://img.shields.io/badge/Documentation-ff69b4)][docs]
[![.github/workflows/branch_main.yml](https://github.com/erlef/purl/actions/workflows/branch_main.yml/badge.svg)](https://github.com/erlef/purl/actions/workflows/branch_main.yml)
[![Coverage Status](https://coveralls.io/repos/github/erlef/purl/badge.svg?branch=main)](https://coveralls.io/github/erlef/purl?branch=main)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/erlef/purl/badge)](https://scorecard.dev/viewer/?uri=github.com/erlef/purl)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/10483/badge)](https://www.bestpractices.dev/projects/10483)

> Implementation of the purl (package url) specification.

See [the documentation][docs].

## Usage

<!-- tabs-open -->

### Elixir

```elixir
iex> Purl.new("pkg:hex/purl")
{:ok, %Purl{type: "hex", name: "purl"}}

iex> Purl.to_string(%Purl{type: "hex", name: "purl"})
"pkg:hex/purl"
```

### Erlang

To handle the purl record, first include the header:
```erlang
-include_lib("purl/include/purl.hrl").
```


```erlang
> purl:new("pkg:hex/purl").
{ok, #purl{type = <<"hex">>, name = <<"purl">>}}

> purl:to_string(#purl{type = <<"hex">>, name = <<"purl">>})
<<"pkg:hex/purl">>
```

<!-- tabs-close -->

## License

Copyright 2023 JOSHMARTIN GmbH  
Copyright 2025 Erlang Ecosystem Foundation

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at:

  > <http://www.apache.org/licenses/LICENSE-2.0>

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

[docs]: https://hexdocs.pm/purl
