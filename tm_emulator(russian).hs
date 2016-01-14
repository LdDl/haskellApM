{-|
@Author:: lopanov@edu
@applied_mathematics
Вопросы присылать на: dima_lopanov@mail.ru / lllnuoh_ogo@mail.ru
-}
{-|
Эмулятор машины Тьюринга (Пример в конце файла)
Для инициализации начального состояния и таблицы:
>> mapM_ print "название машины" без ковычек
mapM - категории Клейсли
mapM_ - категории Клейсли с побочными эффектами монады
Длина ленты: take "длина"
-}
data Step = Step_Left | Step_Right | Stand_Still deriving (Show, Eq)
data Tape a = Tape a [a] [a]
data Movement current_state val = Movement val Step current_state deriving (Show)
 
instance (Show a) => Show (Tape a) where
  show (Tape pos lll rrr) = concat $ left_side ++ [head] ++ right_side
                          where head = "<" ++ show pos ++ ">"
                                left_side = map show $ reverse $ take 15 lll
                                right_side = map show $ take 15 rrr
{-|
Движение каретки
-}
step initialize_rules (current_state, Tape pos (left_to:lll) (right_to:rrr)) = (current_state', tape')
     where  Movement pos' dir current_state' = initialize_rules current_state pos
            tape' = move dir
            move Stand_Still = Tape pos' (left_to:lll) (right_to:rrr)
            move Step_Left = Tape left_to lll (pos':right_to:rrr)
            move Step_Right = Tape right_to (pos':left_to:lll) rrr
run_machine initialize_rules stop start tape = steps ++ [last_step]
      where (steps, last_step:_) = break ((== stop) . fst) $ iterate (step initialize_rules) (start, tape)
 
{-|
Инициализация полосы
-}
tape lam lll rrr | null rrr = Tape lam left_side lam_s
                   | otherwise = Tape (head rrr) left_side right_side
                   where lam_s = repeat lam
                         left_side = reverse lll ++ lam_s
                         right_side = tail rrr ++ lam_s  
 
-- Машина Тьюринга
{-|
Формат составленной машины:
tm "состояние" Значение = Действие Значение Направление_Каретки "Состояние"
и т.д.
НазваниеЛенты = tape "Обозначение пустых ячеек - оно же Лямбда"" [] ["Входные данные"]
Название машины = run_machine tm "Состояние остановки" "Начальное состояние" НазваниеЛенты
Step_Left - Лево.
Step_Right - Право.
Stand_Still - Нейтрально.
Пример:
Алфавит {pos1,pos2...,posN} содержит две буквы. {a,b} ( или {1,2} ).
Пустые ячейки на ленте - 0.
Ввести слово состоящие из букв "a" и "b" (a=1, b=2).
Оставить слово неизменным, если кол-во "a" (1) - нечётно.
Изменить всё слово на слово "bb" (22), если кол-во "a" (1) - чётно.
 
Составленая по условию задачи машина:
Алфавит:     1 ("a") | 2 ("b") | 0 ("лямбда")
Состояния:             |         |
    s1      1.П->1 | 2.П->1  | 0.Л->2
    s2      1.Л->3 | 2.Л->2  | 0.П->4
    s3      1.Л->2 | 2.Л->3  | 0.Н0
    s4      2.П->5 | 2.П->5  | 2.П->5
    s5      2.П->6 | 2.П->6  | 2.П->6
    s6      0.П->6 | 0.П->6  | 0.Н0
Итого таблица будет состоять из 6*3 = 18 строк,
где 6 - это кол-во состояний, а 3 - число букв в алфавите + одна лямбда.
-}
tm "s1" 1 = Movement 1 Step_Right "s1"
tm "s1" 2 = Movement 2 Step_Right "s1"
tm "s1" 0 = Movement 0 Step_Left  "s2"
 
tm "s2" 1 = Movement 1 Step_Left  "s3"
tm "s2" 2 = Movement 2 Step_Left  "s2"
tm "s2" 0 = Movement 0 Step_Right  "s4"
 
tm "s3" 1 = Movement 1 Step_Left  "s2"
tm "s3" 2 = Movement 2 Step_Left  "s3"
tm "s3" 0 = Movement 0 Stand_Still  "Neutral"
 
tm "s4" 1 = Movement 2 Step_Right  "s5"
tm "s4" 2 = Movement 2 Step_Right  "s5"
tm "s4" 0 = Movement 2 Step_Right  "s5"
 
tm "s5" 1 = Movement 2 Step_Right  "s6"
tm "s5" 2 = Movement 2 Step_Right  "s6"
tm "s5" 0 = Movement 2 Step_Right  "s6"
 
tm "s6" 1 = Movement 0 Step_Right  "s6"
tm "s6" 2 = Movement 0 Step_Right  "s6"
tm "s6" 0 = Movement 0 Step_Right  "Neutral"
 
-- Тест для "baabb". Ответ должен быть: "bb"
tape1 = tape 0 [] [2,1,1,2,2]
test1 = run_machine tm "Neutral" "s1" tape1
 
-- Тест для "babb". Ответ должен быть: "babb"
tape2 = tape 0 [] [2,1,2,2]
test2 = run_machine tm "Neutral" "s1" tape2
