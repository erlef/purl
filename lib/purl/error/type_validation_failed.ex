defmodule Purl.Error.TypeValidationFailed do
  @moduledoc """
  Error raised if a field does not conform to the type validation rules of the
  type validation schema.
  """

  @type t :: %__MODULE__{
          type: Purl.type(),
          field: atom(),
          value: String.t(),
          reason: String.t()
        }

  defexception [:type, :field, :value, :reason]

  @impl Exception
  def message(error)

  def message(%__MODULE__{type: type, field: field, value: value, reason: reason}) do
    "Type validation failed for field `#{field}` with value `#{value}` in PURL of type `#{type}`: #{reason}"
  end
end
