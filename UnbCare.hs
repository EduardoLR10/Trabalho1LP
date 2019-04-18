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
{-
comprarMedicamentosDias [] medications num_days = []
comprarMedicamentosDias ((name, horary, next_horary):medication_plane) medications num_days = (name, num_days*length(horary) - quantity):(comprarMedicamentosDias medication_plane medications num_days)
        where
          (n, quantity) = consultarMedicamento name medications
-}
comprarMedicamentosDias medication_plan medications num_days = map toZeroNegatives(map (\ prescription -> (nome (medicamento prescription), num_days*length(horario(prescription)) - quantidade(medicamento prescription))) medication_plan)
  where
  medicamento (n, h, nh)= consultarMedicamento n medications
  nome(n, q) = n
  horario(n, h, nh) = h
  quantidade(n, q) = q
  toZeroNegatives m
    | quantidade m < 0  = (nome m, 0)
    | otherwise         = (nome m, quantidade m)

-- QUESTÂO 10
-- Constrói-se uma lista de todas as compras possíveis em todas as farmácias.
-- Em seguida, determina-se qual dessas compras fica a mais barata.
-- Finalmente, retorna-se o valor final com o nome da farmácia condizente.
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
-- Primeiro, constrói-se uma lista com todas as compras possíveis.
-- O segundo passo é criar uma lista das melhores fármacias para cada medicamento.
-- Finalmente, os remédios que são mais baratos na mesma farmácia são unidos na lista.
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
            unique_purchase = [comprarMedicamentosNaFarmacia [(name, quantity)] farmacy | farmacy <- market, (containAllMeds [(name, quantity)] farmacy) == True]
            only_available = [(price,name) | (price, name) <- unique_purchase, price /= 0]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

medicamentos :: Medicamentos
medicamentos = [("MA", 1),
                ("MB", 2),
                ("MC", 3),
                ("MD", 4)]

hora_atual :: HoraAtual
hora_atual = 08

plano_medicamentos :: PlanoMedicamento
plano_medicamentos = [("MA", [00, 08, 16], 08),
                      ("MB", [00, 12    ], 12),
                      ("MC", [08        ], 08),
                      ("MD", [00, 08, 16], 08)]

mercado :: Mercado
mercado = [("FA", [(("MA", 1), 1),
                   (("MB", 1), 2),
                   (("MC", 1), 3),
                   (("MD", 1), 4)]),
           ("FB", [(("MA", 2), 5),
                   (("MB", 2), 1),
                   (("MC", 2), 2),
                   (("MD", 2), 3)]),
           ("FC", [(("MA", 2), 5),
                   (("MB", 2), 6),
                   (("MC", 2), 1),
                   (("MD", 2), 2),
                   (("ME", 2), 3)]),
           ("FD", [(("MA", 2), 4),
                   (("MB", 2), 5),
                   (("MC", 2), 6),
                   (("MD", 2), 1),
                   (("ME", 2), 2)])]

-- Questão 1 ------------------------------------------------------------------
medicamento_1_1, medicamento_1_2 :: Medicamento
medicamento_1_1 = ("MC", 3)
medicamento_1_2 = ("ME", 5)
{-
adicionarMedicamento medicamento_1_1 medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 6), ("MD", 4)]
adicionarMedicamento medicamento_1_2 medicamentos 
  = [("MA", 1), ("MB", 2), ("MC", 3), ("MD", 4), ("ME", 5)]
-}

-- Questão 2 ------------------------------------------------------------------
nome_2_1, nome_2_2 :: Nome
nome_2_1 = "MC"
nome_2_2 = "ME"
{-
removerMedicamento nome_2_1 medicamentos
  = [("MA", 1), ("MB", 2), ("MD", 4)]
removerMedicamento nome_2_2 medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 3), ("MD", 4)]
-}

-- Questão 3 ------------------------------------------------------------------
nome_3_1, nome_3_2 :: Nome
nome_3_1 = "MC"
nome_3_2 = "ME"
{-
consultarMedicamento nome_3_1 medicamentos
  = ("MC", 3)
consultarMedicamento nome_3_2 medicamentos
  = ("", 0)
-}

-- Questão 4 ------------------------------------------------------------------
medicamento_4_1, medicamento_4_2 :: Medicamento
medicamento_4_1 = ("MC", 10)
medicamento_4_2 = ("ME", 10)
{-
alterarMedicamento medicamento_4_1  medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 10), ("MD", 4)]
alterarMedicamento medicamento_4_2 medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 3), ("MD", 4)]
-}

-- Questão 5 ------------------------------------------------------------------
nome_5_1, nome_5_2 :: Nome
nome_5_1 = "MC"
nome_5_2 = "ME"

{-
tomarMedicamentoSOS nome_5_1 medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 2), ("MD", 4)]
tomarMedicamentoSOS nome_5_2 medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 3), ("MD", 4)]
-}

-- Questão 6 ------------------------------------------------------------------
{-
tomarMedicamentosHorario plano_medicamentos medicamentos hora_atual
  = ([("MA", [0, 8, 16], 16),
      ("MB", [0, 12], 12),
      ("MC", [8], 8),
      ("MD", [0, 8, 16], 16)],
     [("MA", 0), ("MB", 2), ("MC", 2), ("MD", 3)]
    )
-}

-- Questão 7 ------------------------------------------------------------------

{-
cadastrarAlarmes plano_medicamentos
  = [0, 8, 12, 16]
-}

-- Questão 8 ------------------------------------------------------------------
medicamentos_8 :: Medicamentos
medicamentos_8 = [("MA", 1),
                  ("MB", 0),
                  ("MC", 3),
                  ("MD", 0)]

{-
listarMedicamentosComprar medicamentos_8
  = [("MB", 0), ("MD", 0)]
-}

-- Questão 9 ------------------------------------------------------------------
num_dias_9 :: Int
num_dias_9 = 2

{-
comprarMedicamentosDias plano_medicamentos medicamentos num_dias_9
  = medicamentos = [("MA", 5),
                    ("MB", 2),
                    ("MC", 0),
                    ("MD", 2)]
-}

-- Questão 10 -----------------------------------------------------------------
medicamentos_10_1, medicamentos_10_2, medicamentos_10_3 :: Medicamentos
medicamentos_10_1 = [("MA", 1),
                     ("MB", 1),
                     ("MC", 1),
                     ("MD", 1)]
medicamentos_10_2 = [("MA", 2),
                     ("MB", 2),
                     ("MC", 2)]        
medicamentos_10_3 = [("MA", 1),
                     ("MB", 1),
                     ("MC", 1),
                     ("MD", 1),
                     ("ME", 1)]
    
{-
-- O mais barato sendo que todas as farmácias possuem o medicamentos desejado.
comprarMedicamentosPreco medicamentos_10_1 mercado
  = (10, "FA")

-- O mais barato sendo que a que seria mais barata não tem medicamentos suficiente.
comprarMedicamentosPreco medicamentos_10_2 mercado
  = (16, "FB")

-- O mais barato sendo que as duas outras que seriam a mais baratas nem chegam a ofertar todos os medicamentos.
comprarMedicamentosPreco medicamentos_10_3 mercado
  = (17, "FC")
-}

-- Questão 11 -----------------------------------------------------------------

medicamentos_11 :: Medicamentos
medicamentos_11 = [("MA", 2),
                   ("MB", 2),
                   ("MC", 1),
                   ("MD", 2),
                   ("ME", 1)]

{-
comprarMedicamentosPrecoFlex medicamentos_11 mercado
  = [(6, "FB", [("MA", 2), ("MB", 2)]),
     (1, "FC", [("MC", 1)]),
     (4, "FD", [("MD", 2), ("ME", 1)])
    ]
-}