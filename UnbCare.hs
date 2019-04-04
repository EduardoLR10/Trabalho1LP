--module Main where

type Nome = String
type Quantidade = Int
type HorarioProximo = Int
type HoraAtual = Int
type Horario = [Int]
type Medicamento = (Nome,Quantidade)
type Medicamentos = [Medicamento]
type Prescricao = (Nome,Horario,HorarioProximo)
type PlanoMedicamento = [Prescricao]

hora_atual_teste :: HoraAtual
hora_atual_teste = 23
medicamentos_teste :: Medicamentos
medicamentos_teste = [("A", 1), ("B", 2), ("C", 3), ("D", 4), ("ZERO", 0)]
plano_medicamento_teste :: PlanoMedicamento
plano_medicamento_teste = [("A", [23], 23), ("B", [00, 12], 00), ("C", [11, 23], 23), ("D", [11, 23], 23)]

-- QUESTÂO 1
adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
adicionarMedicamento (name, howmany) [] = (name, howmany) : []
adicionarMedicamento (name, howmany) ((n, h):others)
    | name == n     = (n, h + howmany) : others
    | otherwise     = (n, h) : adicionarMedicamento (name, howmany) others

-- QUESTÂO 2
removerMedicamento :: Nome -> Medicamentos -> Medicamentos
removerMedicamento name [] = []
removerMedicamento name ((n, h):others)
    | name == n     = removerMedicamento name others
    | otherwise     = (n, h) : removerMedicamento name others

-- QUESTÂO 3
consultarMedicamento :: Nome -> Medicamentos -> Medicamento
consultarMedicamento name [] = ("", 0)
consultarMedicamento name ((n, h):others)
    | name == n       = (n, h)
    | otherwise       = consultarMedicamento name others

-- QUESTÂO 4
alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
alterarMedicamento (name, howmany) [] = []
alterarMedicamento (name, howmany) ((n, h):others)
    | name == n     = (n, howmany) : others
    | otherwise     = (n, h) : alterarMedicamento (name, howmany) others

-- QUESTÂO 5
tomarMedicamentoSOS  ::  Nome -> Medicamentos ->  Medicamentos
tomarMedicamentoSOS name [] = []
tomarMedicamentoSOS name ((n, h):others)
    | name == n     = (n, h - 1) : others
    | otherwise     = (n, h) : tomarMedicamentoSOS name others

-- QUESTÂO 6
--- Tomar os medicamentos cujo próximo horário seja o mesmo da hora atual, decrementando em um a quantidade existente desses medicamentos tomados.
--- Atualizar o plano de medicamentos configurando o próximo horário de todas as prescrições.
tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)
tomarMedicamentosHorario medication_plan medications current_horary = (medication_plan_f, medications_f)
    where
    medication_names = [n | (n, h, nh) <- medication_plan, nh == current_horary]
    medication_plan_f = atualizarProximoshorarios medication_plan current_horary
    medications_f = tomarMedicamentos medication_names medications

atualizarProximoshorarios :: PlanoMedicamento -> HoraAtual -> PlanoMedicamento
atualizarProximoshorarios [] _       = []
atualizarProximoshorarios ((name, horary, next_horary):medication_plan) current_horary
    | next_horary == current_horary  = (name, horary, selectNextHorary horary current_horary):(atualizarProximoshorarios medication_plan current_horary) 
    | otherwise                      = (name, horary, next_horary):(atualizarProximoshorarios medication_plan current_horary)
    where
    selectNextHorary h ch | [e | e <- h, e > ch] /= []  = minimum [e | e <- h, e > ch]
                          | otherwise                   = minimum h

tomarMedicamentos :: [Nome] -> Medicamentos -> Medicamentos
tomarMedicamentos [] medications = medications
tomarMedicamentos (medication_name:medication_names) medications = tomarMedicamentos medication_names (tomarMedicamentoSOS medication_name medications)

-- QUESTÂO 7
cadastrarAlarmes :: PlanoMedicamento -> Horario
cadastrarAlarmes medication_plan = sortWithoutRepetition(concat [h | (n, h, nh) <- medication_plan])
    where
    sortWithoutRepetition [] = []
    sortWithoutRepetition (a:as) = sortWithoutRepetition [e | e <- as, e < a] ++ [a] ++ sortWithoutRepetition [e | e <- as, e > a]

-- QUESTÂO 8
listarMedicamentosComprar :: Medicamentos ->  Medicamentos
listarMedicamentosComprar medications = [(n, q) | (n, q) <- medications, q <= 0]

{-
main = do
    adicionarMedicamento ("R6", 10) . adicionarMedicamento ("R5", 20) $ remediosteste
    print(adicionarMedicamento ("R7", 10) remediosteste)
    print(removerMedicamento "R2" remediosteste)
    print(consultarMedicamento "R7" remediosteste)
    print(alterarMedicamento ("R7", 20) remediosteste)
    -- print(tomarMedicamentoSOS "R7" remediosteste)
-}