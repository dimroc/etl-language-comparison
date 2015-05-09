defmodule Mapreduce.Mixfile do
  use Mix.Project

  def project do
    [app: :mapreduce,
     version: "0.0.2",
     elixir: "~> 1.0.4 or ~> 1.1-dev",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: []]
  end

  # List all dependencies in the format:
  #
  # { :foobar, git: "https://github.com/elixir-lang/foobar.git", tag: "0.1" }
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    []
  end
end
