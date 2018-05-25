# hive

[![Build Status](https://travis-ci.org/cmc-haskell-2018/hive.svg?branch=master)](https://travis-ci.org/cmc-haskell-2018/hive)

Игра «Улей».
Ветка master - Рабочая версия игры, работает игра против бота и демо-режим
Ветка new_branch_Nargiz - меню игры
Ветка darya_branch - сохранение игры при помощи Enter и загрузка при помощи tab
Сохранения для различных пользователей Различны
Ветка SaurinIndev - дополнительные насекомые из Правил Hive_Carbon, 
устранение багов в перемещении Жука.

## Сборка и запуск

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить проект можно при помощи команды

```
stack build && stack exec hive
```

Запустить игру игрока с ботом

```
stack exec player-vs-bot
```

Запустить игру бота с игроком

```
stack exec bot-vs-player
```

Запустить игру бота с ботом

```
stack exec bot-vs-bot
```

Запустить тесты (которых нет) можно при помощи команды

```
stack test
```

Чтобы запустить интепретатор GHCi и автоматически подгрузить все модули проекта, используйте команду

```
stack ghci
```
![Поле](Поле.PNG)

## Куда тыкать, чтобы заработало

Правила игры: https://s3-eu-west-1.amazonaws.com/mosigra.product.other/481/681/uley.pdf

Нажмите левой кнопкой мыши на фишку, чтобы поднять ее, и на пустую клетку – чтобы поставить.
Если вы хотите выбрать другую фишку для хода, нажмите правую кнопку мыши.

Если фишка поднята, на поле изображаются возможные ходы для нее. Сама фишка рисуется в нижнем углу экрана.

В левом верхнем углу написано, чей сейчас ход. Нельзя взять фишку, цвет которой не соответствует текущему игроку.
Если игра завершилась победой одного из игроков или ничьей, надпись об этом также будет располагаться в левом верхнем углу.

Согласно правилам игры, пчела должна быть поставлена на поле не позже четвертого хода. Если до этого момента пчела не введена в игру, вы будете обязаны ходить ею.

