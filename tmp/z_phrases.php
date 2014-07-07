<?
function phrase($sex,$type,$num,$id) { # id не используется, но если type=1 фраза берется по id из базы 
switch ($type) {
case 1: switch($num) {
		case 1: return ($sex?'вспомнила что-то важное':'вспомнил что-то важное');break;
		case 2: return ($sex?'высморкалась':'высморкался');break;
		case 3: return ($sex?'задумалась':'задумался');break;
		case 4: return ($sex?'замешкалась':'замешкался');break;
		case 5: return ($sex?'засмотрелась на <вырезано цензурой>':'засмотрелся на <вырезано цензурой>');break;
		case 6: return ($sex?'кашлянула':'кашлянул');break;
		case 7: return ($sex?'ковырялась в зубах':'ковырялся в зубах');break;
		case 8: return ($sex?'обернулась':'обернулся');break;
		case 9: return ($sex?'осмотрелась':'осмотрелся');break;
		case 10: return ($sex?'почесалась':'почесался');break;
		case 11: return ($sex?'пошатнулась':'пошатнулся');break;
		case 12: return ($sex?'пришла в себя':'пришел в себя');break;
		case 13: return ($sex?'пыталась сконцентрироваться':'пытался сконцентрироваться');break;
		case 14: return ($sex?'пыталась увернуться':'пытался увернуться');break;
		case 15: return ($sex?'пыталась что-то сказать':'пытался что-то сказать');break;
		case 16: return ($sex?'расплылась в улыбке':'расплылся в улыбке');break;
		case 17: return ($sex?'расстроилась':'расстроился');break;
		case 18: return ($sex?'растерялась':'растерялся');break;
		case 19: return ($sex?'удивилась':'удивился');break;
		case 20: return ($sex?'чесала <вырезано цензурой>':'чесал <вырезано цензурой>');break;
		
		case 21: return ($sex?'пыталась отойти покурить':'пытался отойти покурить');break;
		case 22: return ($sex?'поправляла прическу':'чесал яйца');break;
		case 23: return ($sex?'чесала коко':'чесал коко');break;
		case 24: return ($sex?'':'');break;
		case 25: return ($sex?'':'');break;
		case 26: return ($sex?'':'');break;
		}
break;
case 2: switch($num) {
		case 1: return ($sex?'':'и за это');break;
		case 2: return ($sex?'':'как вдруг');break;
		case 3: return ($sex?'':'а');break;
		case 4: return ($sex?'':'но в это время');break;
		case 5: return ($sex?'':'и тут');break;
		case 6: return ($sex?'':'но вдруг неожиданно');break;
		case 7: return ($sex?'':'и вдруг');break;
		case 8: return ($sex?'':'но');break;
		case 9: return ($sex?'':'и внезапно');break;
		case 10: return ($sex?'':'но неожиданно');break;

		case 11: return ($sex?'':'и в этот момент');break;
		case 12: return ($sex?'':'и в ту же секунду');break;
		case 13: return ($sex?'':'и в этот миг');break;
		case 14: return ($sex?'':'');break;
		case 15: return ($sex?'':'');break;
		case 16: return ($sex?'':'');break;
		}
break;
case 3: switch($num) {
		case 1: return ($sex?'обезумевшая':'обезумевший');break;
		case 2: return ($sex?'расстроенная':'расстроенный');break;
		case 3: return ($sex?'неустрашимая':'неустрашимый');break;
		case 4: return ($sex?'продвинутая':'продвинутый');break;
		case 5: return ($sex?'расстроенная':'расстроенный');break;
		case 6: return ($sex?'страшная':'страшный');break;
		case 7: return ($sex?'задумчивая':'задумчивый');break;
		case 8: return ($sex?'взъерошенная':'небритый');break;
		case 9: return ($sex?'бесстрастная':'бесстрастный');break;
		case 10: return ($sex?'разъяренная':'разъяренный');break;
		case 11: return ($sex?'смелая':'смелый');break;
		case 12: return ($sex?'безумная':'безумный');break;
		case 13: return ($sex?'восхитительная':'восхитительный');break;
		case 14: return ($sex?'жестокая':'жестокий');break;
		case 15: return ($sex?'злобная':'злобный');break;
		case 16: return ($sex?'мужественная':'мужественный');break;
		case 17: return ($sex?'непобедимая':'непобедимый');break;
		case 18: return ($sex?'наглая':'наглый');break;
		case 19: return ($sex?'хитрая':'хитрый');break;
		case 20: return ($sex?'храбрая':'храбрый');break;

		case 21: return ($sex?'отважная':'отважный');break;
		case 22: return ($sex?'железная':'железный');break;
		case 23: return ($sex?'могучая':'могучий');break;
		case 24: return ($sex?'сильнейшая':'сильнейший');break;
		case 25: return ($sex?'крепкая':'крепкий');break;
		case 26: return ($sex?'великая':'великий');break;
		case 27: return ($sex?'бесстрашная':'бесстрашный');break;
		case 28: return ($sex?'':'');break;
		case 29: return ($sex?'':'');break;
		}
break;
case 4: switch($num) {
		case 1: return ($sex?'':'случайно');break;
		case 2: return ($sex?'':'отчаянно');break;
		case 3: return ($sex?'':'проснувшись');break;
		case 4: return ($sex?'':'мимоходом');break;
		case 5: return ($sex?'':'разбежавшись');break;
		case 6: return ($sex?'':'сдуру');break;
		case 7: return ($sex?'':'со скуки');break;
		case 8: return ($sex?'':'не помня себя от испуга');break;
		case 9: return ($sex?'':'улыбаясь');break;
		case 10: return ($sex?'':'прослезившись');break;
		case 11: return ($sex?'':'пошатнувшись');break;
		case 12: return ($sex?'':'проснувшись');break;
		case 13: return ($sex?'':'беспричинно');break;
		case 14: return ($sex?'':'опрометчиво');break;
		case 15: return ($sex?'':'разбежавшись');break;
		case 16: return ($sex?'':'зевнув');break;
		case 17: return ($sex?'':'сделав двойное сальто');break;
		case 18: return ($sex?'':'не подумав');break;
		case 19: return ($sex?'':'хихикая');break;
		case 20: return ($sex?'':'ласково');break;
		case 21: return ($sex?'':'с испугу');break;
		}
break;
case 5: switch($num) {
		case 1: return ($sex?'саданула':'саданул');break;
		case 2: return ($sex?'вломила':'вломил');break;
		case 3: return ($sex?'влепила':'влепил');break;
		}
break;
case 6: switch($num) {
		case 1: return ($sex?'проткнула':'проткнул');break;
		case 2: return ($sex?'уколола':'уколол');break;
		}
break;
case 7: switch($num) {
		case 1: return ($sex?'разрубила':'разрубил');break;
		case 2: return ($sex?'рубанула':'рубанул');break;
		}
break;
case 8: switch($num) {
		case 1: return ($sex?'раздробила':'раздробил');break;
		}
break;
case 9: switch($num) {
		case 1: return ($sex?'рассекла':'рассек');break;
		}
break;
case 10: switch($num) {
		case 1: return ($sex?'':'грубый');break;
		case 2: return ($sex?'':'наглый');break;
		case 3: return ($sex?'':'красивый');break;
		}
break;
case 11: switch($num) {
		case 1: return ($sex?'':'тычок');break;
		}
break;
case 12: switch($num) {
		case 1: return ($sex?'':'рубящий удар');break;
		}
break;
case 13: switch($num) {
		case 1: return ($sex?'':'разбивающий удар');break;
		}
break;
case 14: switch($num) {
		case 1: return ($sex?'':'секущий удар');break;
		case 2: return ($sex?'':'рассекающий удар');break;
		}
break;
case 15: switch($num) {
		case 1: return ($sex?'':'обмораживащий удар');break;
		case 2: return ($sex?'':'морозный удар');break;
		}
break;
case 16: switch($num) {
		case 1: return ($sex?'':'обжигающий удар');break;
		}
break;
case 17: switch($num) {
		case 1: return ($sex?'':'грязным когтем');break;
		case 2: return ($sex?'':'грязными когтями');break;
		case 3: return ($sex?'':'грязными лапами');break;
		case 4: return ($sex?'':'окровавленными когтями');break;
		case 5: return ($sex?'':'острыми когтями');break;
		case 6: return ($sex?'':'открытой ладонью');break;
		case 7: return ($sex?'':'тяжелой лапой');break;
		case 8: return ($sex?'':'увесистой лапой');break;
		}
break;
case 18: switch($num) {
		case 1: return ($sex?'':'молотом');break;
		}
break;
case 19: switch($num) {
		case 1: return ($sex?'':'острым ножом');break;
		}
break;
case 20: switch($num) {
		case 1: return ($sex?'':'посохом');break;
		}
break;
case 21: switch($num) {
		case 1: return ($sex?'':'клинком меча');break;
		case 2: return ($sex?'':'кривым мечом');break;
		case 3: return ($sex?'':'лезвием меча');break;
		case 4: return ($sex?'':'плоской стороной лезвия меча');break;
		case 5: return ($sex?'':'рукояткой меча');break;
		case 6: return ($sex?'':'гардой');break;
		case 7: return ($sex?'':'ножнами');break;
		}
break;
case 22: switch($num) {
		case 1: return ($sex?'':'тяжелой дубиной');break;
		}
break;
case 23: switch($num) {
		case 1: return ($sex?'':'маленьким топорищем');break;
		case 2: return ($sex?'':'занозой на рукоятке топора');break;
		case 3: return ($sex?'':'ручкой топора');break;
		}
break;
case 24: switch($num) {
		case 1: return ($sex?'':'ятаганом');break;
		}
break;
case 25: switch($num) {
		case 1: return ($sex?'':'в правый глаз');break;
		case 2: return ($sex?'':'в нос');break;
		}
break;
case 26: switch($num) {
		case 1: return ($sex?'':'под лопатку');break;
		case 2: return ($sex?'':'в грудь');break;
		case 3: return ($sex?'':'в правый бицепс');break;
		case 4: return ($sex?'':'в грудину');break;
		case 5: return ($sex?'':'в руку');break;
		}
break;
case 27: switch($num) {
		case 1: return ($sex?'':'в правую почку');break;
		case 2: return ($sex?'':'в левую почку');break;
		case 3: return ($sex?'':'в левый бок');break;
		case 4: return ($sex?'':'в по почкам');break;
		case 5: return ($sex?'':'в аппендикс');break;
		case 6: return ($sex?'':'в пресс');break;
		case 7: return ($sex?'':'в мышцы пресса');break;
		}
break;
case 28: switch($num) {
		case 1: return ($sex?'':'в пах');break;
		case 2: return ($sex?'':'по бедрам');break;
		case 3: return ($sex?'':'в чувствительную область паха');break;
		case 4: return ($sex?'':'в копчик');break;
		}
break;
case 29: switch($num) {
		case 1: return ($sex?'':'в икроножную мышцу');break;
		case 2: return ($sex?'':'по ногам');break;
		case 3: return ($sex?'':'в голень');break;
		case 4: return ($sex?'':'в колено');break;
		case 5: return ($sex?'':'в правую ногу');break;
		case 6: return ($sex?'':'в левую ногу');break;
		}
break;
case 30: switch($num) {
		case 1: return ($sex?'':'оппонента');break;
		case 2: return ($sex?'':'врага');break;
		case 3: return ($sex?'':'противника');break;
		case 4: return ($sex?'':'соперника');break;
		}
break;
case 31: switch($num) {
		case 1: return ($sex?'мертва':'мертв');break;
		case 2: return ($sex?'убита':'убит');break;
		case 3: return ($sex?'повержена':'повержен');break;
		case 4: return ($sex?'проиграла бой':'проиграл бой');break;
		}
break;
case 32: switch($num) {
		case 1: return ($sex?'':'начали бой');break;
		case 2: return ($sex?'':'бросили вызов друг другу');break;
		}
break;
case 33: switch($num) {
		case 1: return ($sex?'впала в транс и начала бормотать заклятие':'впал в транс и начал бормотать заклятие');break;
		case 2: return ($sex?'очнулась от медитации, и призвала заклятье':'очнулся от медитации, и призвал заклятье');break;
		case 3: return ($sex?'нарисовав вокруг себя несколько рун, призвала заклятье':'нарисовав вокруг себя несколько рун, призвал заклятье');break;
		case 4: return ($sex?'догадавшись, что пришло время показать себя, произнесла заклятье':'догадавшись, что пришло время показать себя, произнес заклятье');break;
		}
break;
case 34: switch($num) {
		case 1: return ($sex?'нанесла':'нанес');break;
		case 2: return ($sex?'приложила':'приложил');break;
		}
break;
case 35: switch($num) {
		case 1: return ($sex?'':'удар');break;
		}
break;
case 36: switch($num) {
		case 1: return ($sex?'остановила':'остановил');break;
		case 2: return ($sex?'поставила блок на':'поставил блок на');break;
		case 3: return ($sex?'блокировала':'блокировал');break;
		case 4: return ($sex?'отбила':'отбил');break;
		case 5: return ($sex?'остановила':'остановил');break;
		}
break;
case 37: switch($num) {
		case 1: return ($sex?'отбила щитом':'отбил щитом');break;
		}
break;
case 38: switch($num) {
		case 1: return ($sex?'не контролировала ситуацию':'не контролировал ситуацию');break;
		case 2: return ($sex?'обманулась':'обманулся');break;
		case 3: return ($sex?'старалась провести удар':'старался провести удар');break;
		case 4: return ($sex?'думала о <вырезано цензурой>':'думал о <вырезано цензурой>');break;
		case 5: return ($sex?'испугалась':'испугался');break;
		case 6: return ($sex?'потеряла самоконтроль':'потерял самоконтроль');break;
		case 7: return ($sex?'потеряла момент':'потерял момент');break;
		case 8: return ($sex?'была слишком самоуверена':'был слишком самоуверен');break;
		case 9: return ($sex?'пыталась провести удар':'пытался провести удар');break;
		case 10: return ($sex?'поскользнулась':'поскользнулся');break;
		case 11: return ($sex?'не думала о бое':'не думал о бое');break;
		case 12: return ($sex?'не рассчитала свои силы':'не рассчитал свои силы');break;
		case 13: return ($sex?'промахнулась':'промахнулся');break;
		case 14: return ($sex?'оступилась':'оступился');break;
		case 15: return ($sex?'думала не о том':'думал не о том');break;
		}
break;
case 39: switch($num) {
		case 1: return ($sex?'':', и потому');break;
		case 2: return ($sex?'':' и');break;
		case 3: return ($sex?'':', но');break;
		case 4: return ($sex?'':', вследствие чего');break;
		case 5: return ($sex?'':', потому');break;
		}
break;
case 40: switch($num) {
		case 1: return ($sex?'ушла вправо от удара':'ушел вправо от удара');break;
		case 2: return ($sex?'ушла влево от удара':'ушел влево от удара');break;
		case 3: return ($sex?'увела удар':'увел удар');break;
		case 4: return ($sex?'уклонилась от удара':'уклонился от удара');break;
		case 5: return ($sex?'увернулась от удара':'увернулся от удара');break;
		}
break;
case 41: switch($num) {
		case 1: return ($sex?'':'приняв боевые 100 грамм');break;
		case 2: return ($sex?'':'сказав "БУ!"');break;
		case 3: return ($sex?'':'показав противнику кукиш');break;
		case 4: return ($sex?'':'в отчаянном рывке');break;
		case 5: return ($sex?'':'показав сразу два пальца');break;
		}
break;
case 42: switch($num) {
		case 1: return ($sex?'':'о<вырезано цензурой>ный');break;
		case 2: return ($sex?'':'точнейший');break;
		}
break;
case 43: switch($num) {
		case 1: return ($sex?'укусила за пятку противника':'укусил за пятку противника');break;
		case 2: return ($sex?'укусила в ноc':'укусил в ноc');break;
		case 3: return ($sex?'расцарапала нос':'расцарапал нос');break;
		case 4: return ($sex?'ущипнула за мягкое место':'ущипнул за мягкое место');break;
		}
break;
case 44: switch($num) {
		case 1: return ($sex?'':'{datetime} И в этот момент');break;
		case 2: return ($sex?'':'Часы показывали {datetime}, когда');break;
		}
break;
case 45: switch($num) {
		case 1: return ($sex?'что поможет ей только прием':'что поможет ему только прием');break;
		case 2: return ($sex?'что ее спасение это прием':'что его спасение это прием');break;
		#case 2: return ($sex?'что ее спасение это прием':', нетрезво оценив положение, решил, что его спасение это прием');break;
		}
break;
case 46: switch($num) {
		case 1: return ($sex?'поняла, пропустив очередной удар в голову':'понял, пропустив очередной удар в голову');break;
		case 2: return ($sex?', нетрезво оценив положение, решила':', нетрезво оценив положение, решил');break;
		}
break;
case 47: switch($num) {
		case 1: return ($sex?'Кроличья лапка, подкова в перчатке и прием':'Кроличья лапка, подкова в перчатке и прием');break;
		#case 2: return ($sex?', нетрезво оценив положение, решила':', нетрезво оценив положение, решил');break;
		}
break;
case 48: switch($num) {
		case 1: return ($sex?', вспомнив слова своего сэнсея, из последних сил исполнила прием ':', вспомнив слова своего сэнсея, из последних сил исполнил прием ');break;
		}
break;
case 49: switch($num) {
		case 1: return 'Один из бойцов подобрал свиток';break;
		case 2: return 'Один из бойцов нашел свиток и сунул в карман';break;
		case 3: return 'Кое-кто обнаружил что-то интересное на поле боя';break;
		}
break;
case 50: switch($num) {
		case 1: return ($sex?'переманила на свою сторону':'переманил на свою сторону');break;
		case 2: return ($sex?'подманила клона':'подманил клона');break;
		case 3: return ($sex?'прочистила мозги клону':'прочистил мозги клону');break;
		}
break;

#12:04 Кроличья лапка, подкова в перчатке и прием "Призрачная защита" помогли Галлюциногенова продержаться ещё немного.
}
if (!$type) {
	$res=db_use('array',"select * from phrases where id_phrase='".$id."'");
	return ($sex?$res['phrasew']:$res['phrasem']);
}

}
function phrases($type) # количество фраз типа
{
switch ($type) {
case 1: $n=23;break;
case 2: $n=13;break;
case 3: $n=27;break;
case 4: $n=21;break;
case 5: $n=3;break;
case 6: $n=2;break;
case 7: $n=2;break;
case 8: $n=1;break;
case 9: $n=1;break;
case 10: $n=3;break;
case 11: $n=1;break;
case 12: $n=1;break;
case 13: $n=1;break;
case 14: $n=2;break;
case 15: $n=2;break;
case 16: $n=1;break;
case 17: $n=8;break;
case 18: $n=1;break;
case 19: $n=1;break;
case 20: $n=1;break;
case 21: $n=7;break;
case 22: $n=1;break;
case 23: $n=3;break;
case 24: $n=1;break;
case 25: $n=2;break;
case 26: $n=5;break;
case 27: $n=7;break;
case 28: $n=4;break;
case 29: $n=6;break;
case 30: $n=4;break;
case 31: $n=4;break;
case 32: $n=2;break;
case 33: $n=4;break;
case 34: $n=2;break;
case 35: $n=1;break;
case 36: $n=5;break;
case 37: $n=1;break;
case 38: $n=15;break;
case 39: $n=5;break;
case 40: $n=5;break;
case 41: $n=5;break;
case 42: $n=2;break;
case 43: $n=4;break;
case 44: $n=2;break;
case 45: $n=2;break;
case 46: $n=2;break;
case 47: $n=1;break;
case 48: $n=1;break;
case 49: $n=3;break;
case 50: $n=3;break;
}
return $n;
}
function phrase_protknul($type_att) { # проткнул/рассек
	if ($type_att==2) { $a=6;} #колющ
	elseif ($type_att==3) { $a=7;} #рубящ
	elseif ($type_att==4) { $a=8;} #дробящ
	elseif ($type_att==5) { $a=9;} #режущ
	else { $a=5;} #обычный
	return $a;
}
function phrase_type_att($type_att) { # разбивающий удар
	if ($type_att==2) { $a=11;} #колющ
	elseif ($type_att==3) { $a=12;} #рубящ
	elseif ($type_att==4) { $a=13;} #дробящ
	elseif ($type_att==5) { $a=14;} #режущ
	elseif ($type_att==6) { $a=15;} #холод
	elseif ($type_att==7) { $a=16;} #жара
	else{ $a=35;} #обычный
	return $a;
}
function phrase_kuda($kuda){ # в голову / в корпус
	if ($kuda==1) { $a=25;} #голова
	elseif ($kuda==2) { $a=26;} #грудь
	elseif ($kuda==3) { $a=27;} #живот
	elseif ($kuda==4) { $a=28;} #пояс
	elseif ($kuda==5) { $a=29;} #ноги
	return $a;
}
?>