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
medicamentos_teste = [("MA", 1),
                      ("MB", 2),
                      ("MC", 3), 
                      ("MD", 4), 
                      ("M0", 0)]

plano_medicamento_teste :: PlanoMedicamento
plano_medicamento_teste = [("MA", [23    ], 23), 
                           ("MB", [00, 12], 00), 
                           ("MC", [11, 23], 23), 
                           ("MD", [11, 23], 23)]

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

-- QUESTÂO 9
comprarMedicamentosDias :: PlanoMedicamento -> Medicamentos -> Int -> Medicamentos
comprarMedicamentosDias [] medications num_days = []
comprarMedicamentosDias ((name, horary, next_horary):medication_plane) medications num_days = (name, num_days*length(horary) - quantity):(comprarMedicamentosDias medication_plane medications num_days)
    where
    (n, quantity) = consultarMedicamento name medications

-- QUESTÂO 10
type Preco = Int
type Farmacia = (Nome,[(Medicamento,Preco)])
type Mercado = [Farmacia]
type Compra = (Preco, Nome)

mercado_farmacia_teste :: Mercado
mercado_farmacia_teste = [("FA", [(("A", 10), 1), 
                                  (("B", 10), 2), 
                                  (("C", 10), 3), 
                                  (("D", 10), 4)]),
                          ("FB", [(("A", 12), 1),
                                  (("B", 12), 1)]),
                          ("FC", [(("A", 14), 5),
                                  (("B", 14), 8),
                                  (("C", 14), 3)])]

medicamentos_comprados1, medicamentos_comprados2, medicamentos_comprados3 :: Medicamentos
medicamentos_comprados1 = [("A", 1),("B", 1)]             -- comprarMedicamentoPreco: (2, "FB")
medicamentos_comprados2 = [("A", 1), ("B", 1), ("C", 1)]  -- comprarMedicamentoPreco: (6, "FA")
medicamentos_comprados3 = [("A", 13), ("B", 13)]          -- comprarMedicamentoPreco: (169, "FC")

comprarMedicamentosPreco :: Medicamentos -> Mercado -> Compra
comprarMedicamentosPreco medications market = (min_price, min_pharmacy)
    where
    min_price = minimum[p | (p, n) <- comprarMedicamentosNaFarmacia medications market]
    min_pharmacy = head[n, | (p, n) <- comprarMedicamentosNaFarmacia medications market, p == min_price]
    comprarMedicamentosNasFarmacias ms [] = []
    comprarMedicamentosNasFarmacias ms (mkt:mkts)
        | temTodosMedicamentos ms mkt     = (comprarMedicamentosNaFarmacia ms mkt):(comprarMedicamentosNasFarmacias ms mkts)
        | otherwise                       = comprarMedicamentosNasFarmacias ms mkts
    
{-
main = do
    adicionarMedicamento ("R6", 10) . adicionarMedicamento ("R5", 20) $ remediosteste
    print(adicionarMedicamento ("R7", 10) remediosteste)
    print(removerMedicamento "R2" remediosteste)
    print(consultarMedicamento "R7" remediosteste)
    print(alterarMedicamento ("R7", 20) remediosteste)
    -- print(tomarMedicamentoSOS "R7" remediosteste)
-}