module Main where

type Nome = String
type Quantidade = Int
type HorarioProximo = Int
type HoraAtual = Int
type Horario = [Int]
type Medicamento = (Nome,Quantidade)
type Medicamentos = [Medicamento]
type Prescricao = (Nome,Horario,HorarioProximo)
type PlanoMedicamento = [Prescricao]
type Preco = Int
type Farmacia = (Nome,[(Medicamento,Preco)])
type Mercado = [Farmacia]
type Compra = (Preco, Nome)
type CompraFlex = (Preco, Nome, Medicamentos)

hora_atual_teste :: HoraAtual
hora_atual_teste = 12

medicamentos_teste :: Medicamentos
medicamentos_teste = [("MA", 1),
                      ("MB", 2),
                      ("MC", 3), 
                      ("MD", 4),
                      ("ME", 6)]
                      
mercado_farmacia_teste :: Mercado
mercado_farmacia_teste = [("FA", [(("MA", 10), 1), 
                                (("MB", 10), 2), 
                                (("MC", 10), 3),
                                (("MD", 10), 4)]),
                        ("FB", [(("MA", 12), 1),
                                (("MB", 12), 1),
                                (("ME", 12), 10)]),
                        ("FC", [(("MA", 14), 5),
                                (("MB", 14), 8),
                                (("ME", 12), 2),
                                (("MC", 14), 3)])]

plano_medicamento_teste :: PlanoMedicamento
plano_medicamento_teste = [("MA", [6, 12], 12), 
                           ("MB", [00, 12, 18], 12), 
                           ("MC", [11, 12, 23], 23), 
                           ("MD", [11, 12, 13], 12)]
                
farmacia_teste :: Farmacia
farmacia_teste = ("FA", [(("MA", 10), 1), 
                         (("MB", 10), 2), 
                         (("MC", 10), 3), 
                         (("MD", 10), 4)])




medicamentos_comprar1, medicamentos_comprar2, medicamentos_comprar3 :: Medicamentos
medicamentos_comprar1 = [("MA", 1),("MB", 1)]              -- comprarMedicamentoPreco: (2, "FB")
medicamentos_comprar2 = [("MA", 1), ("MB", 1), ("MC", 1)]  -- comprarMedicamentoPreco: (6, "FA")
medicamentos_comprar3 = [("MA", 13), ("MB", 13)]           -- comprarMedicamentoPreco: (169, "FC")

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
    | next_horary == current_horary     = (name, horary, selectNextHorary horary current_horary):(atualizarProximoshorarios medication_plan current_horary) 
    | otherwise                         = (name, horary, next_horary):(atualizarProximoshorarios medication_plan current_horary)
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


comprarMedicamentosPreco :: Medicamentos -> Mercado -> Compra
comprarMedicamentosPreco medications market = compra_final
        where
            compras = [comprarMedicamentosNaFarmacia medications farmacy | farmacy <- market, (containAllMeds medications farmacy) == True]
            melhor_valor = minimum [ price | (price, name) <- compras]
            compra_final = select_by_price compras melhor_valor

containAllMeds :: Medicamentos -> Farmacia -> Bool
containAllMeds medications (name, meds) = foldr (&&) True check
        where
            available = [(meds_name, stock) | ((meds_name, stock), price) <- meds]
            check = [(containOneMed med available) | med <- medications]

containOneMed :: Medicamento -> Medicamentos -> Bool
containOneMed (name, quantity) meds_in_the_farmacy = foldr (||) False check
        where
            check = [name == meds_name && quantity <= stock | (meds_name, stock) <- meds_in_the_farmacy]

select_by_price :: [Compra] -> Preco -> Compra
select_by_price [] _ = (0, "Sem estoque")
select_by_price ((price, name):others) value
    | price == value        = (price, name)
    | otherwise             = select_by_price others value


comprarMedicamentosNaFarmacia :: Medicamentos -> Farmacia -> Compra
comprarMedicamentosNaFarmacia medications (name, stock) = (total_price, name)
        where
            price_medications = concat[unique_price meds_name need stock | (meds_name, need) <- medications]
            
            total_price = foldr (+) 0 price_medications


unique_price :: Nome -> Quantidade -> [(Medicamento,Preco)] -> [Preco]        
unique_price meds_name need stock = [price * need | ((medication_name, quantity), price) <- stock, medication_name == meds_name]

-- QUESTÃO EXTRA 1

comprarMedicamentosPrecoFlex :: Medicamentos -> Mercado -> [CompraFlex]
comprarMedicamentosPrecoFlex medications market = flex_final
        where
            allPurchases = [(unique_meds (name, quantity) market) | (name, quantity) <- medications]
            best_purchase = [find_best_deal compras_por_med | compras_por_med <- allPurchases]
            compra_nome = union_medication best_purchase medications
            compras_flex = [(price, farmacy, [medicamento]) | (price, farmacy, medicamento) <- compra_nome]
            flex_final = union_flex compras_flex
            

union_flex :: [CompraFlex] -> [CompraFlex]
union_flex [] = []
union_flex ((preco, nome, medicamentos):compras) = (soma_precos, nome, lista_medicamentos) : union_flex([(p, n, ms) | (p, n, ms) <- compras , n /= nome])
        where
            soma_precos = foldr (+) 0 [p | (p, n, ms) <- compras, n == nome] + preco
            lista_medicamentos = medicamentos ++ concat([ms | (p, n, ms) <- compras, n == nome])




union_medication :: [Compra] -> Medicamentos -> [(Preco, Nome, Medicamento)]
union_medication [] [] = []
union_medication ((price, farmacy_name):others) (medication:others2) = (price, farmacy_name, medication):union_medication others others2

                
find_best_deal :: [Compra] -> Compra
find_best_deal [] = (0, "Sem estoque")
find_best_deal compra = minimum compra


unique_meds ::  Medicamento -> Mercado -> [Compra]
unique_meds (name, quantity) market = only_available
        where
            unique_purchase = [comprarMedicamentosNaFarmacia [(name, quantity)] farmacy | farmacy <- market]
            only_available = [(price,name) | (price, name) <- unique_purchase, price /= 0]








main = do

    --adicionarMedicamento ("R6", 10) . adicionarMedicamento ("R5", 20) $ remediosteste
    --print(adicionarMedicamento ("R7", 10) remediosteste)
    --print(removerMedicamento "R2" remediosteste)
    --print(consultarMedicamento "R7" remediosteste)
    --print(alterarMedicamento ("R7", 20) remediosteste)
    -- print(tomarMedicamentoSOS "R7" remediosteste)

    --print(tomarMedicamentosHorario plano_medicamento_teste medicamentos_teste 23)
    --print(cadastrarAlarmes plano_medicamento_teste)
    --print(comprarMedicamentosDias plano_medicamento_teste medicamentos_teste 10)
    --print(comprarMedicamentosNaFarmacia medicamentos_teste farmacia_teste)
    print(comprarMedicamentosPreco medicamentos_teste mercado_farmacia_teste)
    print(comprarMedicamentosPrecoFlex medicamentos_teste mercado_farmacia_teste)
