module Main where
import Types
import Data.List

remediosteste = [("R1", 32), ("R2", 55), ("R3", 1), ("R4", 0)]
planoMedicamentoteste = [("R1", [10, 15, 23], 20), ("R2", [9, 13, 00], 20), ("R3", [1, 17, 23], 20)]
horaatualteste = 23

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
tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)
tomarMedicamentosHorario medication_plan medications current_horary 
    | 
    where 
        [n | (n, h, nh) <- medication_plan, elem current_horary h] = 


main = do
    adicionarMedicamento ("R6", 10) . adicionarMedicamento ("R5", 20) $ remediosteste
    print(adicionarMedicamento ("R7", 10) remediosteste)
    print(removerMedicamento "R2" remediosteste)
    print(consultarMedicamento "R7" remediosteste)
    print(alterarMedicamento ("R7", 20) remediosteste)
    -- print(tomarMedicamentoSOS "R7" remediosteste)
