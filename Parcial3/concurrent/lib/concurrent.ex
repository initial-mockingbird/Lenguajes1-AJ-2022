defmodule Concurrent do
  @moduledoc """
  Respuestas de la tarea :D
  """

  @doc """
  Hello world.

  ## Examples

      iex> Concurrent.hello()
      :world

  """
  def hello do
    :world
  end


  @doc """
  zipWithPar.

  zipWith pero cada combinacion se realiza en paralelo
  ## Examples

      iex> Concurrent.zipWithPar([1,2,3],[4,5,6],&(&1+&2))
      [9, 7, 5]

  """
  def zipWithPar(l,r,f) when is_list(l) and is_list(r) do
    zipWithPar_(l,r,f,self(),0)
  end

  def zipWithPar_(l,r,f,pID,n) do
    case {l,r} do
      {[],_} -> []
      {[h1|ls],[h2|rs]}  ->
                IO.inspect({h1,h2},label: "examinando: ")
                spawn(fn -> send(pID,{n,f.(h1,h2)}) end)
                ans = zipWithPar_(ls,rs,f,pID,n+1)
                receive do
                  {^n,sol} -> [sol|ans]
                end
    end
  end

  @doc """
  matrixSum.

  Suma de matrices representadas como listas
  ## Examples

      iex> Concurrent.matrixSum([[1,2,3],[4,5,6]],[[7,8,9],[4,5,6]],&(&1+&2))
      [5,7,9]

  """
  def matrixSum(m1,m2) do
    elementCombine = &(&1 + &2)
    rowCombine     = &(Enum.zip_with(&1,&2,elementCombine))
    zipWithPar(m1,m2,rowCombine)
  end


  @doc """
  pmap.

  map paralelo
  """
  def pmap(collection, func) do
    collection
    |> Enum.map(&(Task.async(fn -> func.(&1) end)))
    |> Enum.map(&Task.await/1)
  end


  def fileExplorer(current) do
    files   = File.ls!(current)
    results = pmap(files, fn f -> n = current <> "/" <> f  ; if is_directory?(n) do fileExplorer(n) else 1 end end)
    Enum.reduce([0|results],&(&1+&2))
  end

  def fileExplorer() do
    fileExplorer(File.cwd!())
  end

  def is_directory?(f) do
    case File.stat!(f) do
      %{:type => :directory} -> true
      _                      -> false
    end
  end

end


m1  = [[1,2,3],[4,5,6],[7,8,9]]
m2  = [[700,800,900],[4,5,6],[1000,2000,3000]]
res = Concurrent.matrixSum(m1,m2)
IO.inspect(res, label: "answer: ")
res = Concurrent.fileExplorer()
IO.inspect(res, label: "answer: ")
