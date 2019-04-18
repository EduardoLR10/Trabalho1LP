module Test where

import UnB
medicamentos = [("MA", 1),
                ("MB", 2),
                ("MC", 3),
                ("MD", 4)]

hora_atual = 08

plano_medicamentos = [("MA", [00, 08, 16], 08),
                      ("MB", [00, 12    ], 12),
                      ("MC", [08        ], 08),
                      ("MD", [00, 08, 16], 08)]
              
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
medicamento_1_1 = ("MC", 3)
medicamento_1_2 = ("ME", 5)
{-
adicionarMedicamento medicamentos medicamento_1_1
  = [("MA", 1), ("MB", 2), ("MC", 3), ("MD", 4)]
adicionarMedicamento medicamentos medicamento_1_2 
  = [("MA", 1), ("MB", 2), ("MC", 3), ("MD", 4), ("ME", 5)]
-}

-- Questão 2 ------------------------------------------------------------------
nome_2_1 = "MC"
nome_2_2 = "ME"
{-
removerMedicamento nome_2_1 medicamentos
  = [("MA", 1), ("MB", 2), ("MD", 4)]
removerMedicamento nome_2_2 medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 3), ("MD", 4)]
-}

-- Questão 3 ------------------------------------------------------------------
nome_3_1 = "MC"
nome_3_2 = "ME"
{-
consultarMedicamento nome_3_1 medicamentos
  = ("MC", 3)
consultarMedicamento nome_3_2 medicamentos
  = ("", 0)
-}

-- Questão 4 ------------------------------------------------------------------
medicamento_4_1 = ("MC", 10)
medicamento_4_2 = ("ME", 10)
{-
alterarMedicamento medicamento_4_1  medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 10), ("MD", 4)]
alterarMedicamento medicamento_4_2 medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 3), ("MD", 4)]
-}

-- Questão 5 ------------------------------------------------------------------
nome_5_1 = "MC"
nome_5_2 = "ME"

{-
tomarMedicamentoSOS nome_5_1 medicamentos
  = [("MA", 1), ("MB", 2), ("MD", 4)]
tomarMedicamentoSOS nome_5_2 medicamentos
  = [("MA", 1), ("MB", 2), ("MC", 3), ("MD", 4)]
-}

-- Questão 6 ------------------------------------------------------------------
{-
tomarMedicamentosHorario plano_medicamentos medicamentos hora_atual
  = ([("MA", [0, 8, 16], 16),
      ("MC", [8], 8),
      ("MB", [0, 12], 12),
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
medicamentos_8 = [("MA", 1),
                  ("MB", 0),
                  ("MC", 3),
                  ("MD", 0)]

{-
listarMedicamentosComprar medicamentos_8
  = [("MB", 0), ("MD", 0)]
-}

-- Questão 9 ------------------------------------------------------------------
num_dias_9 = 2

{-
comprarMedicamentosDias plano_medicamentos medicamentos num_dias_9
  = medicamentos = [("MA", 5),
                    ("MB", 2),
                    ("MC", 0),
                    ("MD", 2)]
-}

-- Questão 10 -----------------------------------------------------------------
--- Testes:
---
---
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
  = ("FA", 10)

-- O mais barato sendo que a que seria mais barata não tem medicamentos suficiente.
comprarMedicamentosPreco medicamentos_10_1 mercado
  = ("FB", 16)

-- O mais barato sendo que as duas outras que seriam a mais baratas nem chegam a ofertar todos os medicamentos.
comprarMedicamentosPreco medicamentos_10_1 mercado
  = ("FC", 17)
-}

-- Questão 11 -----------------------------------------------------------------

medicamentos_11 = [("MA", 2),
                   ("MB", 2),
                   ("MC", 1),
                   ("MD", 2),
                   ("ME", 1)]

{-
comprarMedicamentosPrecoFlex medicamentos_11 mercado
  = [(2, "FB", [("MB", 1)]),
     (2, "FB", [("MB", 2)]),
     (1, "FC", [("MC", 1)]),
     (4, "FD", [("MD", 2), ("ME", 1)])
    ]
-}