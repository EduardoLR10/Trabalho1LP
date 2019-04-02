module Main where
import Types
import Data.List

remediosteste = [("R1", 32), ("R2", 55), ("R3", 1), ("R4", 0)]
planoMedicamentoteste = [("R1", [10, 15, 23], 20), ("R2", [9, 13, 00], 20), ("R3", [1, 17, 23], 20)]
horaatualteste = 23

adicionarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
adicionarMedicamento (name, howmany) [] = (name, howmany) : []
adicionarMedicamento (name, howmany) ((n, h):others)
    | name == n     = (n, h + howmany) : others
    | otherwise     = (n, h) : adicionarMedicamento (name, howmany) others

removerMedicamento :: Nome -> Medicamentos -> Medicamentos
removerMedicamento name [] = []
removerMedicamento name ((n, h):others)
    | name == n     = removerMedicamento name others
    | otherwise     = (n, h) : removerMedicamento name others

consultarMedicamento :: Nome -> Medicamentos -> Medicamento
consultarMedicamento name [] = ("", 0)
consultarMedicamento name ((n, h):others)
    | name == n       = (n, h)
    | otherwise       = consultarMedicamento name others

alterarMedicamento :: Medicamento -> Medicamentos -> Medicamentos
alterarMedicamento (name, howmany) [] = []
alterarMedicamento (name, howmany) ((n, h):others)
    | name == n     = (n, howmany) : others
    | otherwise     = (n, h) : alterarMedicamento (name, howmany) others

tomarMedicamentoSOS  ::  Nome -> Medicamentos ->  Medicamentos
tomarMedicamentoSOS name [] = []
tomarMedicamentoSOS name ((n, h):others)
    | name == n     = (n, h - 1) : others
    | otherwise     = (n, h) : tomarMedicamentoSOS name others

-- tomarMedicamentosHorario :: PlanoMedicamento -> Medicamentos -> HoraAtual -> (PlanoMedicamento,Medicamentos)
-- tomarMedicamentosHorario ((name, (h1:hs), hp): others) ((n, h):others1) time
--     | name == n     = ()


main = do
    adicionarMedicamento ("R6", 10) . adicionarMedicamento ("R5", 20) $ remediosteste
    print(adicionarMedicamento ("R7", 10) remediosteste)
    print(removerMedicamento "R2" remediosteste)
    print(consultarMedicamento "R7" remediosteste)
    print(alterarMedicamento ("R7", 20) remediosteste)
    -- print(tomarMedicamentoSOS "R7" remediosteste)
