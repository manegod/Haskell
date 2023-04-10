apellidos :: [(String, String, Int)] -> [String]

apellidos [] = []
apellidos ((_, surname, _) : xs) = surname: apellidos xs
