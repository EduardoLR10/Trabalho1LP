module Types (
    Nome,
    Quantidade,
    HorarioProximo,
    HoraAtual,
    Horario,
    Medicamento,
    Medicamentos,
    Prescricao,
    PlanoMedicamento
) where

type Nome = String
type Quantidade = Int
type HorarioProximo = Int
type HoraAtual = Int
type Horario = [Int]
type Medicamento = (Nome,Quantidade)
type Medicamentos = [Medicamento]
type Prescricao = (Nome,Horario,HorarioProximo)
type PlanoMedicamento = [Prescricao]