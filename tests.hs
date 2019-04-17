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
                        



