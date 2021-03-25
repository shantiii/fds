defmodule Fds.MixProject do
  use Mix.Project

  def project do
    [
      app: :fds,
      version: "0.2.1",
      elixir: "~> 1.8",
      erlc_options: erlc_options(Mix.env()),
      start_permanent: Mix.env() == :prod,
      description: "Erlang/Elixir's missing fast / functional data structures",
      package: package(),
      deps: [], #YO.
      test_paths: ["test"],
      test_pattern: "fds/*.exs",
      #language: :erlang,
      default_task: "compile",
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [ extra_applications: [:logger] ]
  end

  defp package do
    [
      name: "fds",
      files: ~w(LICENSE README.md mix.exs src lib priv),
      licenses: ["Apache-2.0"],
      links: %{"GitHub" => "https://github.com/shantiii/fds"},
    ]
  end

  defp erlc_options(:test), do: [d: :EUNIT, d: :TEST]
  defp erlc_options(_env), do: []
end
