<?php

//ini_set("display_errors", 1);

ob_start("ob_gzhandler");

	session_start();
	if (!($_SESSION['uid'] >0)) {header("Location: index.php"); mysql_query('UPDATE `users` SET `hp2` = 0,`hp3` = 0,`hit` = 0,`s_duh` = 0,`krit` = 0,`counter` = 0,`block2` = 0,`parry` = 0  WHERE `id` = '.$_SESSION['uid'].''); 
mysql_query("DELETE FROM `person_on` WHERE `id_person`='".$_SESSION['uid']."'");}
	include './connect.php';
	// ставим блокировку на таблицу

	mysql_query("LOCK TABLES `bots` WRITE, `puton` WRITE, `priem` WRITE, `shop` WRITE, `person_on` WRITE, `podzem3` WRITE, `canal_bot` WRITE, `labirint` WRITE, `battle` WRITE, `logs` WRITE, `users` WRITE, `inventory` WRITE, `magic` WRITE, `effects` WRITE, `clans` WRITE, `online` WRITE;");

	$user = mysql_fetch_array(mysql_query("SELECT * FROM `users` WHERE `id` = '{$_SESSION['uid']}' LIMIT 1;"));
	$klan = mysql_fetch_array(mysql_query("SELECT name, clanexp FROM `clans` WHERE `name` = '{$user['klan']}' LIMIT 1;"));
	include './functions.php';
$lpp=rand(0,5);
if($lpp==5){
	include './cron.php';
}
	if ($_POST['end'] != null) {header("Location: main.php"); mysql_query('UPDATE `users` SET `hp2` = 0,`hp3` = 0,`hit` = 0,`s_duh` = 0,`krit` = 0,`counter` = 0,`block2` = 0,`parry` = 0  WHERE `id` = '.$_SESSION['uid'].''); 
mysql_query("DELETE FROM `person_on` WHERE `id_person`='".$_SESSION['uid']."'"); }
if(!$_SESSION['btime']){$chk=time()-$_SESSION['btime'];}
if($chk && $chk<=2 && $_SESSION['btime']){ unset($attack); unset($defend); unset($_POST['attack']); unset($_POST['defend']);}else{$_SESSION['btime']=time(); }

// ========================================================================================================================================================
// начало исполняемого кода битвы
//=========================================================================================================================================================
$s_duh=$user['s_duh'];$hit=$user['hit'];$krit=$user['krit'];$block=$user['block2'];$parry=$user['parry'];$hp=$user['hp2'];$counter=$user['counter'];
$chkwear1 = mysql_query('SELECT id FROM `inventory` WHERE (`type` = 3 AND `dressed` = 1) AND `owner` = '.$user['id'].';');
while ($chkwear = mysql_fetch_array($chkwear1)) {
$sumwear++;
}

class prieminfo{
  var $id_priem;
  var $name;
  var $type;
  var $priem;
  var $n_block;
  var $n_counter;
  var $n_hit;
  var $n_hp;
  var $n_krit;
  var $n_parry;
  var $minlevel;
  var $wait;
  var $maxuses;
  var $minhp;
  var $sduh_proc;
  var $sduh;
  var $hod;
  var $intel;
  var $mana;
  var $opisan;
  var $m_magic1;
  var $m_magic2;
  var $m_magic3;
  var $m_magic4;
  var $m_magic5;
  var $m_magic6;

  var $m_magic7;
  var $needsil;
  var $needvyn;
  function prieminfo($s,$priem) { # либо по id ($s) либо по названию $priem
    if ($s) {
$res=mysql_fetch_array (mysql_query("select * from priem where id_priem='".$s."';"));
  //    $res=db_use('array',"select * from priem where id_priem='".$s."';");
    }else{
$res=mysql_fetch_array (mysql_query("select * from priem where priem='".$priem."';"));
 //     $res=db_use('array',"select * from priem where priem='".$priem."';");
    }
    $this->id_priem=$res['id_priem'];#$id_priem;
    $this->name=$res['name'];
    $this->type=$res['type'];
    $this->priem=$res['priem'];
    $this->n_block=$res['n_block'];
    $this->n_counter=$res['n_counter'];
    $this->n_hit=$res['n_hit'];
    $this->n_hp=$res['n_hp'];
    $this->n_krit=$res['n_krit'];
    $this->n_parry=$res['n_parry'];
    $this->minlevel=$res['minlevel'];
    $this->wait=$res['wait'];
    $this->maxuses=$res['maxuses'];
    $this->minhp=$res['minhp'];
    $this->sduh_proc=$res['sduh_proc'];
    $this->sduh=$res['sduh'];
    $this->hod=$res['hod'];
    $this->intel=$res['intel'];
    $this->mana=$res['mana'];
    $this->opisan=$res['opisan'];
    $this->m_magic1=$res['m_magic1'];
    $this->m_magic2=$res['m_magic2'];
    $this->m_magic3=$res['m_magic3'];
    $this->m_magic4=$res['m_magic4'];
    $this->m_magic5=$res['m_magic5'];
    $this->m_magic6=$res['m_magic6'];
    $this->m_magic7=$res['m_magic7'];
    $this->needsil=$res['need_sil'];
    $this->needvyn=$res['need_vyn'];
  }
  function check_hars($n) {
global $user; # проверка. n=0: все хар-ки n=1: только жизнь и мана
    if($n==0) {

      if(($this->minlevel<=$user['level']) && ($this->intel<=$user['intel']) && ($this->needsil<=$user['sila']) && ($this->needvyn<=$user['vinos']) && ($this->m_magic1<=$user['mfire']) && ($this->m_magic2<=$user['mwater']) && ($this->m_magic3<=$user['mair']) && ($this->m_magic4<=$user['mearth']) && ($this->m_magic5<=$user['mlight']) && ($this->m_magic6<=$user['mgray']) && ($this->m_magic7<=$user['mdark']) ) {
        return true;
      }else{return false;}
    }elseif($n==1){
      if($this->check_hars(0) && ($this->mana<=$user['mana']) && ($this->minhp<=$user['hp'])) {
        return true;     # !!!!!!!!!!!!!!!! НЕ ДОДЕЛАНО !!!!!!!!!!!!!!!!!!!!!!
      }else{return false;}
    }
  }                        
  function checkbattlehars($myinfo,$hit,$krit,$parry,$counter,$block,$s_duh,$hp) { # влад магией, статы + хар-ки битвы
global $user;
$s_duh = floor($s_duh/100);
  if (
  $hit>=$this->n_hit &&
  $krit>=$this->n_krit &&
  $parry>=$this->n_parry &&
  $counter>=$this->n_counter &&
  $hp>=$this->n_hp &&
  $block>=$this->n_block &&
  $user['level']>=$this->minlevel &&
  $user['hp']>=$this->minhp &&
  $s_duh>=$this->sduh &&
  $user['intel']>=$this->intel &&
  $user['mana']>=$this->mana &&
  $user['mfire']>=$this->m_magic1 &&
  $user['mwater']>=$this->m_magic2 &&
  $user['mair']>=$this->m_magic3 &&
  $user['mearth']>=$this->m_magic4 &&
  $user['mlight']>=$this->m_magic5 &&
  $user['mgray']>=$this->m_magic6 &&
  $user['mdark']>=$this->m_magic7 &&
  $user['sila']>=$this->needsil &&
  $user['vinos']>=$this->needvyn ) {
mysql_query("update person_on set `pr_active`= 1 WHERE id_person='".$_SESSION['uid']."' and pr_name='".$this->priem."' and `pr_active` < 2");
  return true;}


/*echo"

  ".$hit.">=".$this->n_hit." &&
  ".$krit.">=".$this->n_krit." &&
  ".$parry.">=".$this->n_parry." &&
  ".$counter.">=".$this->n_counter." &&
  ".$hp.">=".$this->n_hp." &&
  ".$block.">=".$this->n_block." &&
  ".$user['level'].">=".$this->minlevel." &&
  ".$user['hp'].">=".$this->minhp." &&
  (".$s_duh." && (".$s_duh.">=".$this->sduh." OR ".$this->sduh_proc.")) &&
  ".$user['intel'].">=".$this->intel." &&
  ".$user['mana'].">=".$this->mana." &&
  ".$user['mfire'].">=".$this->m_magic1." &&
  ".$user['mwater'].">=".$this->m_magic2." &&
  ".$user['mair'].">=".$this->m_magic3." &&
  ".$user['mearth'].">=".$this->m_magic4." &&
  ".$user['mlight'].">=".$this->m_magic5." &&
  ".$user['mgray'].">=".$this->m_magic6." &&
  ".$user['mdark'].">=".$this->m_magic7." &&
  ".$user['sila'].">=".$this->needsil." &&
  ".$user['vinos'].">=".$this->needvyn.""

;*/
  }
  }


class ActivePriems {
  var $priems;
  function ActivePriems($id_person){
   // $res=db_use('query',"select * from person_on where type=3 and id_person='".$id_person."'");
$res= mysql_query("select pr_active,pr_cur_uses,pr_wait_for,pr_name from person_on where type=3 and id_person='".$id_person."'");

    $arr=array ();
    unset($i);while ($i<mysql_num_rows($res)) {
      $i++;$s=mysql_fetch_array($res);
      $arr[$s['pr_name']]['active']=$s['pr_active'];
      $arr[$s['pr_name']]['uses']=$s['pr_cur_uses'];
      $arr[$s['pr_name']]['wait']=$s['pr_wait_for'];
    }
    $this->priems=$arr;
}
}


	if ($_GET['uszver'] && $user['zver_id']>0) {


	    $zver=mysql_fetch_array(mysql_query("SELECT * FROM `users` WHERE `id` = '{$user['zver_id']}' LIMIT 1;"));
    if($zver){
	if($zver['sitost']>=3){
		$nb = mysql_fetch_array(mysql_query("SELECT id FROM `bots` WHERE `name` LIKE '".$zver['login']."';"));
		if(!$nb){
		mysql_query("INSERT INTO `bots` (`name`,`prototype`,`battle`,`hp`) values ('".$zver['login']."','".$zver['id']."','".$user['battle']."','".$zver['hp']."');");
		$bot = mysql_insert_id();
		
		$bd = mysql_fetch_array(mysql_query ('SELECT * FROM `battle` WHERE `id` = '.$user['battle'].' LIMIT 1;'));
		$battle = unserialize($bd['teams']);
		$battle[$bot] = $battle[$user['id']];
		foreach($battle[$bot] as $k => $v) {
			$battle[$k][$bot] = array(0,0,time());
		}
		$t1 = explode(";",$bd['t1']);		
		if (in_array ($user['id'],$t1)) {$ttt = 1;} else {	$ttt = 2;}					
		addlog($user['battle'],'<span class=date>'.date("H:i").'</span> '.nick5($user['id'],"B".$ttt).' призвал своего зверя '.nick5($bot,"B".$ttt).'<BR>');
					
		mysql_query('UPDATE `battle` SET `teams` = \''.serialize($battle).'\', `t'.$ttt.'`=CONCAT(`t'.$ttt.'`,\';'.$bot.'\')  WHERE `id` = '.$user['battle'].' ;');
		
		mysql_query("UPDATE `battle` SET `to1` = '".time()."', `to2` = '".time()."' WHERE `id` = ".$user['battle']." LIMIT 1;");							
				
		$bet=1;
		echo "Ваш зверь призван в бой.";
		
		}else{echo "Ваш зверь уже был призван в бой.";}
		}else{echo "Ваш зверь слишком голодный.";}
		}else{echo "У вас нет зверя!";}




}




	if ($_GET['special']) {

		$priem2=str_replace(array('"',"'","\\"),array('','',''),$_GET['special']);

$res=mysql_query("select slot,id_thing from puton where id_person='".$_SESSION['uid']."' and slot>=201 and slot<=210;");

 while ($s=mysql_fetch_array($res)) {
$res4=mysql_fetch_array(mysql_query("select priem from priem where id_priem='".$s['id_thing']."';"));
    $puton[$s['slot']]=$res4['priem'];
  }
$igogo=new ActivePriems($_SESSION['uid']);


for ($i=201;$i<=210;$i++) {
if($puton[$i]==$priem2){$priem=$priem2; break;} 
}
	
$have_priem=true;$p=&new prieminfo(0,$priem);

	
		if ($have_priem) {
			# проверить можно ли
			$enable=true;
			# проверить по статам
			if ($p->checkbattlehars($myinfo,$hit,$krit,$parry,$counter,$block,$s_duh,$hp)) {

				# можно ипользовать если: прошел срок wait - это в случае если задан wait в параметрах приема
                                $act=&$igogo->priems[$priem];
				if ($p->wait) {
					if ($act['wait']>0) {
						$enable=false;echo "<font color=red>нельзя использовать: еще идет задержка </font>";
					}
					if ($act['active']!=1) {$enable=false;echo "<font color=red>нельзя использовать: уже активен</font>";}

				}else{
					if ($act['active']!=1) {$enable=false;echo "<font color=red>нельзя использовать: уже активен</font>";}
				}
			}else{
				$enable=false;echo "<font color=red>нельзя использовать: не хватает требований</font>";
			}
			if ($enable) {
				$ok=false;
				switch ($priem) {
					case 'parry_prediction':$ok=true;
					case 'krit_blindluck':$ok=true;
					case 'krit_wildluck':$ok=true;
					case 'hit_overhit':$ok=true;
					case 'hit_strong':$ok=true;
					case 'block_activeshield':$ok=true;
					case 'hit_luck':$ok=true;
					case 'block_fullshield':$ok=true;
					case 'block_absolute':$ok=true;
					case 'hp_regen':$ok=true;
					case 'hit_willpower':$ok=true;
					case 'novice_hp':$ok=true;
					case 'novice_def':$ok=true;
					case 'novice_hit':$ok=true;
					case 'wis_fire_incenerate04':$ok=true;
					case 'wis_fire_incenerate05':$ok=true;
					case 'wis_fire_incenerate06':$ok=true;
					case 'wis_fire_incenerate07':$ok=true;
					case 'wis_fire_incenerate08':$ok=true;
					case 'wis_fire_incenerate09':$ok=true;
					case 'wis_fire_incenerate10':$ok=true;
					case 'wis_fire_incenerate11':$ok=true;
					case 'wis_air_shaft04':$ok=true;
					case 'wis_air_shaft05':$ok=true;
					case 'wis_air_shaft06':$ok=true;
					case 'wis_air_shaft07':$ok=true;
					case 'wis_air_shaft08':$ok=true;
					case 'wis_air_shaft09':$ok=true;
					case 'wis_air_shaft10':$ok=true;
					case 'wis_air_shaft11':$ok=true;
					case 'wis_water_frost04':$ok=true;
					case 'wis_water_frost05':$ok=true;
					case 'wis_water_frost06':$ok=true;
					case 'wis_water_frost07':$ok=true;
					case 'wis_water_frost08':$ok=true;
					case 'wis_water_frost09':$ok=true;
					case 'wis_water_frost10':$ok=true;
					case 'wis_water_frost11':$ok=true;
					case 'wis_earth_rain05':$ok=true;
					case 'wis_earth_rain06':$ok=true;
					case 'wis_earth_rain07':$ok=true;
					case 'wis_earth_rain08':$ok=true;
					case 'wis_earth_rain09':$ok=true;
					case 'wis_earth_rain10':$ok=true;
					case 'wis_earth_rain11':$ok=true;
				}
				if ($ok) {if ($debug) {echo $priem;}
				#1) $act['active'] - активен -> UPDATE
				#2) $act['active']<1 - неактивен НО есть $act['uses']>0 -> UPDATE
				#3) $act['uses'] <1 -> INSERT
					if ($act['uses']>0) {
						mysql_query("update person_on set pr_cur_uses=pr_cur_uses+1".($p->wait?',wait='.$p->wait:'')."
						WHERE id_person='".$_SESSION['uid']."' and type=3 and pr_name='".$priem."'");
					}
                                         /////здесь будут заюзываться приемы/////
                                         include 'incl/usepriems.php';

		/*			$myinfo->priems->priems[$priem]['active']=1;
					$myinfo->priems->priems[$priem]['uses']++;
					# обновить что потратили
					db_use('query',"update battle_units set s_duh=".($p->sduh?'s_duh-'.$p->sduh:'s_duh-s_duh*'.(0+$p->sduh_proc)).",
					hit=hit-'".(0+$p->n_hit)."',krit=krit-'".(0+$p->n_krit)."',counter=counter-'".(0+$p->n_counter)."',
					block=block-'".(0+$p->n_block)."',parry=parry-'".(0+$p->n_parry)."',hp=hp-'".(0+$p->n_hp)."'
					WHERE id_person='".$myinfo->id_person."' and id_battle='".$id_battle."'");
					$s_duh=$s_duh-($p->sduh?$p->sduh:$s_duh*$p->sduh_proc);
					$hit=$hit-$p->n_hit;$krit=$krit-$p->krit;$counter=$counter-$p->n_counter;$block=$block-$p->n_block;
					$parry=$parry-$p->n_parry;$hp=$hp-$p->n_hp; */
				}#/ok
			}#enable
		}else{
			echo "<font color=red>нет такого приема на персонаже</font>";
		}
	}






$s_duh=$user['s_duh'];$hit=$user['hit'];$krit=$user['krit'];$block=$user['block2'];$parry=$user['parry'];$hp=$user['hp2'];$counter=$user['counter'];












	class fbattle {
			public $mysql = null; // идентификатор мускуль сессии
			public $status = integer; // статус сражения  ---- 0 - нет битвы; 1 - есть битва
			public $battle = array(); //массив с драчующимися
			public $battle_data = array(); // данные по битве
			public $enemy = null; // идентификатор противника
			public $damage = array(); // массив с нанесенным уроном
			public $t1 = array(); // первая команда
			public $t2 = array(); // вторая команда
			public $team_enemy = array(); // команда противника (ссылка на тимсы)
			public $team_mine = array(); // своя команда
			public $user = array(); // инфа на игрока
			public $enemyhar = array(); // инфа на противника
			public $enemy_dress = array(); // шмот врага
			public $user_dress = array(); // свой  шмот
			public $en_class, $my_class; // цвета для лога
			public $bots = array (); public $botsid = array ();//Массив с ботами
			public $log = ""; // агреггатор лога
			public $to1; public $to2; //таймауты
            public $exp = array(); // экспа
			public $log_debug = "";
                      
                    

/*-------------------------------------------------------------------
 создание класса и сбор основной инфы
--------------------------------------------------------------------*/
			function fbattle ($battle_id) {
				global $mysql, $user, $_POST, $textp;

				// соединяем мускуль и юзера
				$this->mysql = $mysql;
				$this->user = $user;
				// перебираем варианты
				if ($battle_id > 0) {
					// ставим статус битвы на "есть битва"
					$this->status = 1;
					// вставляем драчующихся
					$this->battle_data = mysql_fetch_array(mysql_query ('SELECT * FROM `battle` WHERE `id` = '.$battle_id.' LIMIT 1;'));
                                        if($this->battle_data['room']<=0){mysql_query('UPDATE `battle` SET `room` ='.$user['room'].'  WHERE `id` = '.$battle_id.';');}
					$this->sort_teams();
					// получили дамагу
					$this->damage = unserialize($this->battle_data['damage']);
					// кто драчуется?
					$this->battle = unserialize($this->battle_data['teams']);
					// получаем экспу
					$this->exp = unserialize($this->battle_data['exp']);
					// таймі
					$this->to1 = $this->battle_data['to1'];
					$this->to2 = $this->battle_data['to2'];

					// ============СИСТЕМА БОТОВ=================
					$bots = mysql_query ('SELECT * FROM `bots` WHERE `battle` = '.$battle_id.' AND `hp` > 0;');
					while ($bot = mysql_fetch_array($bots)) {
						$this->bots[$bot['id']] = $bot;
						// листаем противников, и выставляем удары для живых персонажей
						if($bot['hp'] > 0) {
							foreach ($this->battle[$bot['id']] as $k => $v) {
								if($this->battle[$bot['id']][$k][0] == 0 && $k < 10000000) {
									mt_srand(microtime(true));
	
									$chkwear_bot1 = mysql_query('SELECT id FROM `inventory` WHERE (`type` = 3 AND `dressed` = 1) AND `owner` = '.$bot['prototype'].';');
									while ($chkwear_bot = mysql_fetch_array($chkwear_bot1)) {
									$sumwear_bot++;
									}
									if($sumwear_bot==2){$udar2=rand(1,5);}else{$udar2=0;}
									$this->battle[$bot['id']][$k] = array(rand(1,5),rand(1,5),time(),$udar2);
									$this->battle[$k][$bot['id']] = array(0,0,time(),0);
								}
								if($this->battle[$k][$bot['id']][0] == 0 && $k < 10000000) {
									if(in_array($user['id'],array_keys($this->battle[$bot['id']]))) {
									//echo "111";
										// если я противник бота
										if ($this->my_class=='B2') {
											if($this->to2 <= $this->to1) {
												$endr= ((time()-$this->to2) > $this->battle_data['timeout']*60);
											}
										} else {
											if($this->to2 >= $this->to1) {
												$endr= ((time()-$this->to1) > $this->battle_data['timeout']*60);
											}
										}
										if($endr && !$uje) {
											$uje = true;
											// если сдул - заканчиваем бой
											$this->add_log("<span class=date>".date("H:i")."</span> Бой закончен по таймауту.<BR>");

											//$this->write_log ();
											foreach ($this->battle[$bot['id']] as $k => $v) {
												if($k > _BOTSEPARATOR_) {
													$bots = mysql_fetch_array(mysql_query ('SELECT `hp` FROM `bots` WHERE `id` = '.$k.' LIMIT 1;'));
													$us['hp'] = $bots['hp'];
												} else {
													$us = mysql_fetch_array(mysql_query('SELECT `hp` FROM `users` WHERE `id` = '.$k.' LIMIT 1;'));
												}
												if($us && (int)$us['hp']>0) {
													$tr = settravma($k,0,86400,1);
													$this->add_log('<span class=date>'.date("H:i").'</span> '.nick7($k).' получил повреждение: <font color=red>'.$tr.'</font><BR>');
												}
											}
											//$this->write_log ();
											foreach ($this->battle[$bot['id']] as $k => $v) {
												mysql_query('UPDATE users SET `hp` =0, `fullhptime` = '.time().' WHERE `id` = '.$k.';');
												mysql_query('UPDATE users SET `mana` =0, `fullmptime` = '.time().' WHERE `id` = '.$k.';');
																	}
										}
									}
								}
							}
							$this->update_battle ();
						}
					}
					//==============================================

					if($_POST['enemy'] > 0) {
						// ударяемся
						$this->razmen_init ($_POST['enemy'],$_POST['attack'],$_POST['defend'],$_POST['attack1']);
						if($user['room']!=403 && $user['room']!=20){include "astral.php";}
						header ("Location:main.php");
					}
					else
					{
						$this->sort_teams();
						$this->fast_death();

						// вібираем вражину
						$this->enemy = (int)$this->select_enemy();
						if($this->enemy > 0) {
							// табличка удара-блока
							$this->return = 1;
						}
						else {
							//проверяем тайм
							if ($this->get_timeout() && $this->user['hp'] > 0) {
								// табличка тайма
								$this->return = 3;
							}
							else {
								// ожидаем хода...
								$this->return = 2;
							}
						}
					}
					if ($_POST['victory_time_out2']) {
						$this->end_draft();
					}
					if ($_POST['victory_time_out']) {
						$this->end_gora();
					}
					if ($this->battle_end()) {
						$this->return = 2;
					}

					$this->write_log(); // пишем лог
					$this->write_debug(); // пишем лог

					return $this->return;
				}
				else {
					// ставим статус битвы на "нет битвы"
					$this->status = 0;
					//header ("Location:main.php");
					//die();
					//$this->return = 5;
					//return $this->return;
				}
			}
/*-------------------------------------------------------------------
  проверка и выставление конца боя
--------------------------------------------------------------------*/
			function battle_end	() {
			if($this->battle_data) {
				$ss = @array_keys($this->battle);

					$t1life = 0;
					$t2life = 0;
					// проверяем живность команд
					foreach ($this->t1 as $k => $v) {
						if (in_array($v,array_keys($this->battle))) {
							$t1life++;
						}
					}
					foreach ($this->t2 as $k => $v) {
						if (in_array($v,array_keys($this->battle))) {
							$t2life++;
						}
					}
			if($t2life == 0 OR $t1life == 0) {
				$charge = mysql_fetch_array(mysql_query ('SELECT `win` FROM `battle` WHERE `id` = '.$this->battle_data['id'].' LIMIT 1;'));
			}
			if(($t2life == 0 OR $t1life == 0) && ($charge[0] == 3 || $charge[0] == 9)) {

				// ============================= конец боя ==========================
					mysql_query("UPDATE battle SET `win` = 0 WHERE `id` = {$this->user['battle']}");

					// оцениваем игроков
					//$cost1 =0; $cost2 =0; $kula4ka = 0; $t2c =0; $t1c =0; $lvs1=0; $lvs2=0; $bxp = 0;
					foreach ($this->t1 as $k => $v) {
						/*if($v > _BOTSEPARATOR_) {
							$bots = mysql_fetch_array(mysql_query ('SELECT * FROM `bots` WHERE `id` = '.$v.' LIMIT 1;'));
							$gamer = mysql_fetch_array(mysql_query("select 1+IFNULL((select SUM(cost) FROM inventory WHERE owner = users.id AND dressed=1),0), `align`,`level` FROM users WHERE id = ".$bots['prototype']." LIMIT 1;"));
							$kulak = mysql_fetch_array(mysql_query("select SUM(cost) FROM inventory WHERE owner = ".$bots['prototype']." AND dressed=1 LIMIT 1;"));
						} else {
							$gamer = mysql_fetch_array(mysql_query("select 1+IFNULL((select SUM(cost) FROM inventory WHERE owner = users.id AND dressed=1),0), `align`,`level` FROM users WHERE id = ".$v." LIMIT 1;"));
							$kulak = mysql_fetch_array(mysql_query("select SUM(cost) FROM inventory WHERE owner = ".$v." AND dressed=1 LIMIT 1;"));
						}  */
						$nks1[] = nick7($v);
						$nks1hist[] = nick3($v);
						/*$td1 += $this->damage[$v];

						$bxp += $baseexp[$gamer[2]];
						$bxp1 += $baseexp[$gamer[2]];
						//$exp1 += $this->damage[$v];

						$cost1 += $gamer[0];
						$kula4ka += $kulak[0];
						$al1 = $gamer[1];
						$t1c ++;
						$lvs1 += $gamer[2];  */
					}
				//	$lvs1 = $lvs1/$t1c+1;
					foreach ($this->t2 as $k => $v) {
						/*if($v > _BOTSEPARATOR_) {
							$bots = mysql_fetch_array(mysql_query ('SELECT * FROM `bots` WHERE `id` = '.$v.' LIMIT 1;'));
							$gamer = mysql_fetch_array(mysql_query("select 1+IFNULL((select SUM(cost) FROM inventory WHERE owner = users.id AND dressed=1),0), `align`,`level` FROM users WHERE id = ".$bots['prototype']." LIMIT 1;"));
							$kulak = mysql_fetch_array(mysql_query("select SUM(cost) FROM inventory WHERE owner = ".$bots['prototype']." AND dressed=1 LIMIT 1;"));
						} else {
							$gamer = mysql_fetch_array(mysql_query("select 1+IFNULL((select SUM(cost) FROM inventory WHERE owner = users.id AND dressed=1),0), `align`,`level` FROM users WHERE id = ".$v." LIMIT 1;"));
							$kulak = mysql_fetch_array(mysql_query("select SUM(cost) FROM inventory WHERE owner = ".$v." AND dressed=1 LIMIT 1;"));
						}    */
						$nks2[] = nick7($v);
						$nks2hist[] = nick3($v);
						/*$td2 += $this->damage[$v];

						$bxp += $baseexp[$gamer[2]];
						$bxp2 += $baseexp[$gamer[2]];
						//$exp2 += $this->damage[$v];

						$cost2 += $gamer[0];
						$kula4ka += $kulak[0];
						$al2 = $gamer[1];
						$lvs2 += $gamer[2];
						$t2c++;  */
					}
					/*$lvs2 = $lvs2/$t2c+1;
					//echo mysql_error();
					//echo $cost1,' ',$cost2;
					if(($t1c==1) && ($t2c==1)) {
						$one2one=true;
						// одинаковые ли склонности?
						if ($al1==3 && ($al2 > 1 && $al2 < 2)) { $dual = true; }
						if ($al2==3 && ($al1 > 1 && $al1 < 2)) { $dual = true; }
					} else {
						$one2one=false;
					}
                    */
					// тима победителей
					if(in_array($ss[0],$this->t1)) {
						$flag = 1;
						foreach ($this->t1 as $k => $v) {
							mysql_query('UPDATE `battle` SET `win` = 1 WHERE `id` = '.$this->user['battle'].' ;');
							$this->t1[$k] = nick5($v," ");

								$this->exp[$v] = round($this->exp[$v]);

///////////////////////при победе = для подземки/////////////////////////////////////
$gess = mysql_query ('SELECT * FROM `labirint` WHERE `user_id` = '.$this->user['id'].'');
if($hokke = mysql_fetch_array($gess)){
$glav_id = $hokke["glav_id"];
$glava = $hokke["glava"];
$nm = $hokke["boi"];
/////////////////////////////////////////////////////////////
$DR = mysql_fetch_array(mysql_query("SELECT * FROM `canal_bot` WHERE `glava`='$glava' and `boi`= '$nm'"));
if($DR){
$bot = $DR["bot"];
$nomer = $DR["nomer"];
////////////////////////////////////////////////////////////////
$shans1 = rand(0,100);
$shans2 = rand(0,100);
$shans3 = rand(0,100);
////////////////////////////////////////////////////////////////
$est=0;$d1=0;$d2=0;
if($bot=='1' or $bot=='2' or $bot=='3' or $bot=='1.1' or $bot=='1.2' or $bot=='1.3' or $bot=='2.2' or $bot=='2.3' or $bot=='3.3' or $bot=='1.1.1' or $bot=='1.1.2' or $bot=='1.1.2' or $bot=='1.2.2' or $bot=='1.3.2' or $bot=='1.3.3' or $bot=='2.2.2' or $bot=='2.2.3' or $bot=='2.3.3' or $bot=='3.3.3' or $bot=='1.3.2'){
if($bot=='1' and $bot=='2' and $bot=='3')
{if($shans1<'50'){$d1=1;} }
if($bot=='1.1' or $shans2<'50' and $bot=='1.2' or $shans2<'50' and $bot=='1.3' or $shans2<'50' and $bot=='2.2' or $shans2<'50' and $bot=='2.3' or $shans2<'50' and $bot=='3.3')
{if($shans1<'50'){$d1=1;}if($shans2<'50'){$d2=1;}}
if($bot=='1.1.1' or $shans3<'50' and $bot=='1.1.2' or $shans3<'50' and $bot=='1.1.2' or $shans3<'50' and $bot=='1.2.2' or $shans3<'50' and $bot=='1.3.2' or $shans3<'50' and $bot=='1.3.3' or $shans3<'50' and $bot=='2.2.2' or $shans3<'50' and $bot=='2.2.3' or $shans3<'50' and $bot=='2.3.3' or $shans3<'50' and $bot=='3.3.3' or $shans3<'50' and $bot=='1.3.2')
{if($shans1<'50'){$d1=1;}if($shans2<'50'){$d2=1;}if($shans3<'50'){$d3=1;}}
$est = $d1+$d2+$d3+500;
if($est>'500'){mysql_query("UPDATE podzem3 SET n$nomer='$est' WHERE glava='$glava' and name='".$hokke["name"]."'");}
else{mysql_query("UPDATE podzem3 SET n$nomer='' WHERE glava='$glava' and name='".$hokke["name"]."'");}
}


if($bot=='4' or $bot=='5' or $bot=='6' or $bot=='8'){
if($shans1<'99'){$est=504;}
if($est>'500'){mysql_query("UPDATE podzem3 SET n$nomer='$est' WHERE glava='$glava' and name='".$hokke["name"]."'");}
else{mysql_query("UPDATE podzem3 SET n$nomer='' WHERE glava='$glava' and name='".$hokke["name"]."'");}
}

if($bot=='7'){
if($shans1<'99'){$est=510;}
if($est=='510'){mysql_query("UPDATE podzem3 SET n$nomer='$est' WHERE glava='$glava' and name='".$hokke["name"]."'");}
else{mysql_query("UPDATE podzem3 SET n$nomer='' WHERE glava='$glava' and name='".$hokke["name"]."'");}
if($this->user['medal2']=='0'){mysql_query("UPDATE `users` SET `medal2`='1' WHERE `id`=".$this->user['id']."");}
}
//////////////////////////2 etaz/////////////////////////////////////////////////////////////////
if($bot=='9' or $bot=='11' or $bot=='9.9' or $bot=='11.11' or $bot=='9.9.9' or $bot=='11.11.11'){

if($bot=='9' or $bot=='11'){if($shans1<'99'){$d1=1;} }
if($bot=='9.9' or $bot=='11.11'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} }
if($bot=='9.9.9' or $bot=='11.11.11'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} if($shans3<'99'){$d3=1;} }
$est = $d1+$d2+$d3+600;
if($est>'600'){mysql_query("UPDATE podzem3 SET n$nomer='$est' WHERE glava='$glava' and name='".$hokke["name"]."'");}
else{mysql_query("UPDATE podzem3 SET n$nomer='' WHERE glava='$glava' and name='".$hokke["name"]."'");}

}//////////Чистая гайка////////////////////////////////////////////////////
if($bot=='13' or $bot=='13.13' or $bot=='13.13.13'){

if($bot=='13'){if($shans1<'99'){$d1=1;} }
if($bot=='13.13'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} }
if($bot=='13.13.13'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} if($shans3<'99'){$d3=1;} }
$est = $d1+$d2+$d3+603;
if($est>'603'){mysql_query("UPDATE podzem3 SET n$nomer='$est' WHERE glava='$glava' and name='".$hokke["name"]."'");}
else{mysql_query("UPDATE podzem3 SET n$nomer='' WHERE glava='$glava' and name='".$hokke["name"]."'");}

}//////////Гайка с резьбой////////////////////////////////////////////////////
if($bot=='10' or $bot=='10.10' or $bot=='10.10.10'){

if($bot=='10'){if($shans1<'99'){$d1=1;} }
if($bot=='10.10'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} }
if($bot=='10.10.10'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} if($shans3<'99'){$d3=1;} }
$est = $d1+$d2+$d3+606;
if($est>'606'){mysql_query("UPDATE podzem3 SET n$nomer='$est' WHERE glava='$glava' and name='".$hokke["name"]."'");}
else{mysql_query("UPDATE podzem3 SET n$nomer='' WHERE glava='$glava' and name='".$hokke["name"]."'");}

}//////////длинный болт////////////////////////////////////////////////////
if($bot=='12' or $bot=='12.12' or $bot=='12.12.12' or $bot=='15' or $bot=='15.15' or $bot=='15.15.15' or $bot=='16' or $bot=='16.16' or $bot=='16.16.16'){

if($bot=='12' or $bot=='15' or $bot=='16'){if($shans1<'99'){$d1=1;} }
if($bot=='12.12' or $bot=='15.15' or $bot=='16.16'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} }
if($bot=='12.12.12' or $bot=='15.15.15' or $bot=='16.16.16'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} if($shans3<'99'){$d3=1;} }
$est = $d1+$d2+$d3+609;
if($est>'609'){mysql_query("UPDATE podzem3 SET n$nomer='$est' WHERE glava='$glava' and name='".$hokke["name"]."'");}
else{mysql_query("UPDATE podzem3 SET n$nomer='' WHERE glava='$glava' and name='".$hokke["name"]."'");}

}//////////Нужный болт////////////////////////////////////////////////////
if($bot=='14' or $bot=='14.14' or $bot=='14.14.14' or $bot=='17' or $bot=='17.17' or $bot=='17.17.17' or $bot=='18' or $bot=='18.18' or $bot=='18.18.18'){

if($bot=='14' or $bot=='17' or $bot=='18'){if($shans1<'99'){$d1=1;} }
if($bot=='14.14' or $bot=='17.17' or $bot=='18.18'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} }
if($bot=='14.14.14' or $bot=='17.17.17' or $bot=='18.18.18'){if($shans1<'99'){$d1=1;} if($shans2<'99'){$d2=1;} if($shans3<'99'){$d3=1;} }
$est = $d1+$d2+$d3+612;
if($est>'612'){mysql_query("UPDATE podzem3 SET n$nomer='$est' WHERE glava='$glava' and name='".$hokke["name"]."'");}
else{mysql_query("UPDATE podzem3 SET n$nomer='' WHERE glava='$glava' and name='".$hokke["name"]."'");}

}//////////Рабочий винтель////////////////////////////////////////////////////
}
mysql_query("UPDATE `labirint` SET `boi`='0' WHERE `user_id`=".$this->user['id']."");
mysql_query("DELETE FROM `canal_bot` WHERE `nomer`='$nomer' and `glava`='$glava' and `boi`='$nm'");
}
/////////////////////////////////////////////////////////////////////////////////////////////////////
if($this->user['align']==4){$proc_exp=floor(proc_exp/2);}else{$proc_exp=proc_exp;}
$zv=mysql_fetch_array(mysql_query("SELECT `prototype`,`id` FROM `bots` WHERE `prototype` = '".$this->user['zver_id']."' and `battle` = ".$this->user['battle'].""));
if($zv){
$proc_exp=floor(($proc_exp/3)*2);
$id_bota = $zv['id'];
//$esp = $this->damage[$id_bota];
  $esp = floor($this->exp[$v]/2);
if($esp<0){$esp='0';}
mysql_query('UPDATE `users` SET `exp` = `exp`+'.$esp.',`sitost` = `sitost`-3 WHERE `user_id` = '.$v.';');
addchp ('<font color=red>Внимание!</font> Ваш зверь получил <b>'.$esp.'</b> опыта.   ','{[]}'.nick7 ($v).'{[]}');
}
if ($this->user['klan']) {
						$klanexp1 = floor($this->exp[$v]/5);	
							addchp ('<font color=red>Внимание!</font> Бой закончен. Всего вами нанесено урона: <b>'.$this->damage[$v].' HP</b>. Получено кланового опыта: <b>'.$klanexp1.'</b>.  Получено опыта: <b>'.$this->exp[$v].'</b> ('.$proc_exp.'%).','{[]}'.nick7 ($v).'{[]}');
mysql_query('UPDATE `users` SET `hp2` = 0,`hp3` = 0,`hit` = 0,`s_duh` = 0,`krit` = 0,`counter` = 0,`block2` = 0,`parry` = 0,`udar` = 0  WHERE `id` = '.$v.''); 
mysql_query("DELETE FROM `person_on` WHERE `id_person`='".$v."'");
mysql_query("UPDATE `effects` SET `isp` = '0' WHERE `owner` = '".$v."'");
}else{
		addchp ('<font color=red>Внимание!</font> Бой закончен. Всего вами нанесено урона: <b>'.$this->damage[$v].' HP</b>. Получено опыта: <b>'.$this->exp[$v].'</b> ('.$proc_exp.'%).','{[]}'.nick7 ($v).'{[]}');
mysql_query('UPDATE `users` SET `hp2` = 0,`hp3` = 0,`hit` = 0,`s_duh` = 0,`krit` = 0,`counter` = 0,`block2` = 0,`parry` = 0,`udar` = 0  WHERE `id` = '.$v.''); 
mysql_query("DELETE FROM `person_on` WHERE `id_person`='".$v."'");
mysql_query("UPDATE `effects` SET `isp` = '0' WHERE `owner` = '".$v."'");
}
				$vrag_w = mysql_fetch_array(mysql_query("SELECT `name` FROM `bots` WHERE  `id` = ".$v." LIMIT 1 ;"));
				if($vrag_w['name']=="Общий Враг"){mysql_query('UPDATE users SET `win`=`win` +1 WHERE `id` = 23;');}

		    				mysql_query('UPDATE users SET `win`=`win` +1, `exp` = `exp`+'.$this->exp[$v].', `fullhptime` = '.time().', `fullmptime` = '.time().',`udar` = "0" WHERE `id` = '.$v.';');
							//mysql_query("UPDATE clans SET `clanexp`=`clanexp`+10 WHERE `name` =  '{$this->user['klan']}';");						   
							$klanexp = $this->exp[$this->user['id']]/5;				
							mysql_query("UPDATE clans SET `clanexp`=`clanexp`+'{$klanexp}' WHERE `name` =  '{$this->user['klan']}';");						   
							}
						$winers .= implode("</B>, <B>",$this->t1);
						$lomka=$this->t2;
					} elseif(in_array($ss[0],$this->t2)) {
						$flag = 2;
						foreach ($this->t2 as $k => $v) {
							mysql_query('UPDATE `battle` SET `win` = 2 WHERE `id` = '.$this->user['battle'].' ;');
							$this->t2[$k] = nick5($v,"");

							    $this->exp[$v] = round($this->exp[$v]);
////////////////при пройгрыше = для поземки//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
$sd=mysql_query("SELECT glav_id,boi,glava,dead FROM `labirint` WHERE `user_id`=".$this->user['id']." and `di`='0'");
if($dd=mysql_fetch_array($sd)){
$glav_id = $dd["glav_id"];
$glava = $dd["glava"];
$nm = $dd["boi"];
mysql_query("DELETE FROM `canal_bot` WHERE `boi`='$nm' and `glava`='$glava'");
if($dd["dead"]=='0'){$d = 1;}
if($dd["dead"]=='1'){$d = 2;}
if($dd["dead"]=='2'){$d = 3;}
mysql_query("UPDATE `labirint` SET `location`='16',`vector`='0',`dead`='$d',`t`='226',`l`='454',`boi`='0',`di`='1',`name`='Канализация 1 этаж' WHERE `user_id`=".$this->user['id']."");
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
if($this->user['align']==4){$proc_exp=floor(proc_exp/2);}else{$proc_exp=proc_exp;}
$zv=mysql_fetch_array(mysql_query("SELECT `prototype`,`id` FROM `bots` WHERE `prototype` = '".$this->user['zver_id']."' and `battle` = ".$this->user['battle'].""));
if($zv){
$proc_exp=floor(($proc_exp/3)*2);
$id_bota = $zv['id'];
//$esp = $this->damage[$id_bota];
  $esp = floor($this->exp[$v]/2);
if($esp<0){$esp='0';}
mysql_query('UPDATE `users` SET `exp` = `exp`+'.$esp.',`sitost` = `sitost`-3 WHERE `user_id` = '.$v.';');
addchp ('<font color=red>Внимание!</font> Ваш зверь получил <b>'.$esp.'</b> опыта.   ','{[]}'.nick7 ($v).'{[]}');
}
							addchp ('<font color=red>Внимание!</font> Бой закончен. Всего вами нанесено урона: <b>'.(int)$this->damage[$v].' HP</b>. Получено опыта: <b>'.$this->exp[$v].'</b> ('.$proc_exp.'%).  ','{[]}'.nick7 ($v).'{[]}');
                                                        mysql_query('UPDATE `users` SET `hp2` = 0,`hp3` = 0,`hit` = 0,`s_duh` = 0,`krit` = 0,`counter` = 0,`block2` = 0,`parry` = 0,`udar` = 0  WHERE `id` = '.$v.'');
                                                        mysql_query("DELETE FROM `person_on` WHERE `id_person`='".$v."'");
														mysql_query("UPDATE `effects` SET `isp` = '0' WHERE `owner` = '".$v."'");
				$vrag_w = mysql_fetch_array(mysql_query("SELECT `name` FROM `bots` WHERE  `id` = ".$v." LIMIT 1 ;"));
				if($vrag_w['name']=="Общий Враг"){mysql_query('UPDATE users SET `win`=`win` +1 WHERE `id` = 23;');}
							mysql_query('UPDATE users SET `win`=`win`+1, `exp` = `exp`+'.$this->exp[$v].', `fullhptime` = '.time().', `fullmptime` = '.time().',`udar` = "0" WHERE `id` = '.$v.';');
		    				}
						$winers .= implode("</B>, <B>",$this->t2);
						$lomka=$this->t1;
					}

					mysql_query("UPDATE `users`, `bots` SET `users`.`fullhptime` = ".(time()+300).",`users`.`hp` = `bots`.`hp` WHERE `users`.id=83 AND `bots`.prototype=83;");


					// ===================ломаем шмот=============
					if ($lomka) {
						foreach ($lomka as $k => $v) {
							if (rand(1,3)==1){
								$us = mysql_query('UPDATE `inventory` SET `duration`=`duration`+1 WHERE `type` <> 25 AND `dressed` = 1 AND `owner` = \''.$v.'\';');
							}
							$this->exp[$v] = 0;
							addchp ('<font color=red>Внимание!</font> Бой закончен. Всего вами нанесено урона: <b>'.(int)$this->damage[$v].' HP</b>. Получено опыта: <b>0</b>.   ', '{[]}'.nick7 ($v).'{[]}');
                                                        mysql_query('UPDATE `users` SET `hp2` = 0,`hp3` = 0,`hit` = 0,`s_duh` = 0,`krit` = 0,`counter` = 0,`block2` = 0,`parry` = 0 ,`udar` = 0 WHERE `id` = '.$v.'');
							mysql_query("DELETE FROM `person_on` WHERE `id_person`='".$v."'");
							mysql_query("UPDATE `effects` SET `isp` = '0' WHERE `owner` = '".$v."'");
				$vrag_w = mysql_fetch_array(mysql_query("SELECT `name` FROM `bots` WHERE  `id` = ".$v." LIMIT 1 ;"));
				if($vrag_w['name']=="Общий Враг"){mysql_query('UPDATE users SET `lose`=`lose` +1 WHERE `id` = 23;');}
							mysql_query('UPDATE `users` SET `lose`=`lose` +1 WHERE `id` = \''.$v.'\';');
							// если поединок был кровавым - еще и ставм травмы

						}
					}

					foreach ($this->t1 as $k => $v) {
								$us = mysql_query('SELECT duration, maxdur, name FROM `inventory` WHERE `type` <> 25 AND `dressed` = 1 AND `owner` = \''.$v.'\';');
								while ($rrow=mysql_fetch_row($us)) {
									if (($rrow[1]-$rrow[0])==1)
										$this->add_log('<span class=date>'.date("H:i").'</span> Внимание! У "'.nick7($v).'" предмет "'.$rrow[2].'" в критическом состоянии! <BR><small>(на правах рекламы) <b>Ремонтная мастерская Newkombats</b>. Мы даем вторую жизнь старым вещам!</small><BR>');
									elseif (($rrow[1]-$rrow[0])==2)
										$this->add_log('<span class=date>'.date("H:i").'</span> Внимание! У "'.nick7($v).'" предмет "'.$rrow[2].'" нуждается в ремонте! <BR><small>(на правах рекламы) <b>Ремонтная мастерская Newkombats</b>. Мы даем вторую жизнь старым вещам!</small><BR>');
									}
						}
					foreach ($this->t2 as $k => $v) {
								$us = mysql_query('SELECT duration, maxdur, name FROM `inventory` WHERE `type` <> 25 AND `dressed` = 1 AND `owner` = \''.$v.'\';');
								while ($rrow=mysql_fetch_row($us)) {
									if (($rrow[1]-$rrow[0])==1)
										$this->add_log('<span class=date>'.date("H:i").'</span> Внимание! У "'.nick7($v).'" предмет '.$rrow[2].' в критическом состоянии! <BR><small>(на правах рекламы) <b>Ремонтная мастерская Newkombats</b>. Мы даем вторую жизнь старым вещам!</small><BR>');
									elseif (($rrow[1]-$rrow[0])==2)
										$this->add_log('<span class=date>'.date("H:i").'</span> Внимание! У "'.nick7($v).'" предмет "'.$rrow[2].'" нуждается в ремонте! <BR><small>(на правах рекламы) <b>Ремонтная мастерская Newkombats</b>. Мы даем вторую жизнь старым вещам!</small><BR>');
									}
						}

					//==============================================
					if($winers) {
						$this->add_log('<span class=date>'.date("H:i").'</span> '.'Бой закончен, победа за <B>'.$winers.'</B><BR>');
		   				 	if($this->battle_data['blood']) {
							$this->add_log('<span class=date>'.date("H:i").'</span> ... и победители стали калечить проигравших...<BR>');
							foreach ($lomka as $k => $v) {
                                                     if($this->battle_data['blood']==2) {
								$tr = settravma($v,13,86400,1); 
                                                     }
                                                         else{
                                                                $tr = settravma($v,0,86400,1); 

                                                      }
								$this->add_log('<span class=date>'.date("H:i").'</span> '.nick7($v).' получил повреждение: <font color=red>'.$tr.'</font><BR>');
							}
						}
					} else {
						$this->add_log('<span class=date>'.date("H:i").'</span> '.'Бой закончен. Ничья.<BR>');
                $vrag_w = mysql_fetch_array(mysql_query("SELECT `name` FROM `bots` WHERE  `battle` = ".$this->user['battle']." LIMIT 1 ;"));
                if($vrag_w['name']=="Общий Враг"){mysql_query('UPDATE users SET `nich`=`nich` +1 WHERE `id` = 23;');}
						mysql_query("UPDATE users SET `battle` =0, `nich` = `nich`+'1',`fullhptime` = ".time().",`fullmptime` = ".time().",`udar` = '0' WHERE `battle` = {$this->user['battle']}");						
						$this->exp = null;
////////////////при Ничьей = для поземки///////////////////
$sd=mysql_query("SELECT glav_id,boi,glava FROM `labirint` WHERE `user_id`=".$this->user['id']."");
if($dd=mysql_fetch_array($sd)){
$glav_id = $dd["glav_id"];
$glava = $dd["glava"];
$nm = $dd["boi"];
mysql_query("DELETE FROM `canal_bot` WHERE `boi`='$nm' and `glava`='$glava'");
mysql_query("UPDATE `labirint` SET `location`='16',`vector`='0',`dead`=dead+1,`t`='226',`l`='454',`boi`='0' WHERE `user_id`=".$this->user['id']."");
}
///////////////////////////////////
mysql_query("UPDATE `effects` SET `isp` = '0' WHERE `owner` = '".$this->user['id']."'");
					
					}

					// sys
					if ($flag==1) {
						$rr = implode("</B>, <B>",$nks1)."<img src=i/flag.gif></B> и <B>".implode("</B>, <B>",$nks2);
					} elseif ($flag==2) {
						$rr = implode("</B>, <B>",$nks1)."</B> и <B>".implode("</B>, <B>",$nks2)."<img src=i/flag.gif>";
					} else {
						$rr = implode("</B>, <B>",$nks1)."</B> и <B>".implode("</B>, <B>",$nks2)."";
					}
					// выносим хп-ку и выходим из боя

					mysql_query('UPDATE `battle` SET `t1hist` = \''.implode(", ",$nks1hist).'\', `t2hist` = \''.implode(", ",$nks2hist).'\' WHERE `id` = '.$this->battle_data['id'].' ;');
					addch ("<a href=logs.php?log=".$this->battle_data['id']." target=_blank>Поединок</a> между <B>".$rr."</B> закончен.   ",$user['room']);
					mysql_query('UPDATE `battle` SET `exp` = \''.serialize($this->exp).'\' WHERE `id` = '.$this->battle_data['id'].' ;');
					mysql_query("DELETE FROM `bots` WHERE `battle` = {$this->user['battle']};");
					mysql_query("UPDATE users SET `battle` =0, `fullhptime` = ".time().", `fullmptime` = ".time()." WHERE `battle` = {$this->user['battle']}");

					unset($this->battle);
					//header("Location: fbattle.php");	die();
					return true;
				// =================================================================
				}
			}
				return false;
			}
/*-------------------------------------------------------------------
  gora - я победил
--------------------------------------------------------------------*/
			function end_gora() {
				// я - царь горы
					if ($this->get_timeout()) {
						//$this->add_log("<span class=date>".date("H:i")."</span> Бой закончен по таймауту.<BR>");
						//$this->write_log ();

                        foreach ($this->team_mine as $v) {
                        	 if (in_array($v,array_keys($this->battle))) {
                                  $vvv = $v;
                                  // $this->add_log("<BR>".$v);
                        	 }
                        }
                        $this->add_log("<span class=date>".date("H:i")."</span> Бой закончен по таймауту.<BR>");


						foreach ($this->team_enemy as $v => $k) {
							if($k > _BOTSEPARATOR_) {
								$bots = mysql_fetch_array(mysql_query ('SELECT `hp` FROM `bots` WHERE `id` = '.$k.' LIMIT 1;'));
								$us['hp'] = $bots['hp'];
							} else {
								$us = mysql_fetch_array(mysql_query('SELECT `hp` FROM `users` WHERE `id` = '.$k.' LIMIT 1;'));
							}
							if($us && (int)$us['hp']>0) {
								if(!$this->battle_data['blood']) {
									$tr = settravma($k,0,86400,1);
									$this->add_log('<span class=date>'.date("H:i").'</span> '.nick7($k).' получил повреждение: <font color=red>'.$tr.'</font><BR>');
								}
							}
						}
						//$this->write_log ();
						foreach ($this->team_enemy as $v => $k) {
							mysql_query('UPDATE users SET `hp` =0, `fullhptime` = '.time().', `fullmptime` = '.time().' WHERE `id` = '.$k.';');
						}
						header("Location:".$_SERVER['PHP_SELF']."?batl=".$this->user['battle']);
					}
			   }

/*-------------------------------------------------------------------
  draft - ничья
--------------------------------------------------------------------*/
			function end_draft() {
				//foreach ($this->battle[$this->user['id']] as $k => $v) {
	            if(!$this->user['in_tower']) {
					if ($this->get_timeout()) {
						$this->battle = null;
						mysql_query("UPDATE users SET `battle` =0, `nich` = `nich`+'1',`fullhptime` = ".time().",`fullmptime` = ".time()." WHERE `battle` = {$this->user['battle']}");
                                                      mysql_query('UPDATE `users` SET `hp2` = 0,`hp3` = 0,`hit` = 0,`s_duh` = 0,`krit` = 0,`counter` = 0,`block2` = 0,`parry` = 0  WHERE `battle` = '.$this->user['battle'].'');
						      mysql_query("DELETE FROM `person_on` WHERE `id_person`='".$this->user['battle']."'");
						$this->add_log("<span class=date>".date("H:i")."</span> Бой закончен по таймауту. Ничья.<BR>");
						mysql_query("UPDATE battle SET `win` = 0 WHERE `id` = {$this->user['battle']}");
						$this->exp = null;
						$this->write_log ();
					}
				}
			}
/*-------------------------------------------------------------------
 мочим трупов
--------------------------------------------------------------------*/
			function fast_death() {
				// убиваем трупов
				if($this->battle) {
					//$this->battle[$this->user['id']]=1;
					foreach($this->battle as $k=>$v) {
						if($k > _BOTSEPARATOR_) {
							$bots = mysql_fetch_array(mysql_query ('SELECT * FROM `bots` WHERE `id` = '.$k.' LIMIT 1;'));
							$us = mysql_fetch_array(mysql_query('SELECT `hp`, `maxhp`, `sex`,`id`,`battle` FROM `users` WHERE `id` = '.$bots['prototype'].' LIMIT 1;'));
							$us['hp'] = $bots['hp'];
							$us['battle'] = $bots['battle'];
						} else {
							$us = mysql_fetch_array(mysql_query('SELECT `hp`, `maxhp`, `sex`,`id`,`battle` FROM `users` WHERE `id` = '.$k.' LIMIT 1;'));
						}
						if($us && (int)$us['hp']<=0) {
						 //$battle_data = mysql_fetch_array(mysql_query ('SELECT * FROM `battle` WHERE `id` = '.$this->user['battle'].' LIMIT 1;'));
						 //$war = unserialize($battle_data['teams']);
						// unset($battle_data);
						 //$war=array_keys($war);
						// if(in_array($k,$war)) {
							unset($this->battle[$k]);
								if($us['id']==23){
					$battle_datav = mysql_fetch_array(mysql_query ('SELECT t1 FROM `battle` WHERE `id` = '.$us['battle'].' LIMIT 1;'));
 					$t1v = explode(";",$battle_datav['t1']);
				foreach ($t1v as $ff => $ll) {
		$zashc = mysql_fetch_array(mysql_query("SELECT name FROM `effects` WHERE `owner` = ".$ll." and `type`=395 limit 1;"));
if(!$zashc){ 
		mysql_query("INSERT INTO `effects` (`owner`,`name`,`time`,`type`) values ('".$ll."','Знак Защитника Клуба',".(time()+2592000).",395);");
}else{		        mysql_query("UPDATE `effects` set `time` = '".(time()+2592000)."' WHERE `owner` = ".$ll." AND `type`=395");
}							
								}
           
								}
							if($us['sex'] == 1) {
								//$blablа = array('побежден великим войном по имени ','повержен ударом воителя, известного под именем ','проиграл бой с небольшой помощью от бойца, известного в этом мире, как','погиб от удара бойца');
								$this->add_log('<span class=sysdate>'.date("H:i").'</span> '.nick5($k,'b').' проиграл!<BR>');
							} else {
								//$blabla1 = array('побеждена великим войном по имени ','повержена ударом воителя, известного под именем ','проиграла бой с небольшой помощью от бойца, известного в этом мире, как','погибла от удара бойца');
								$this->add_log('<span class=sysdate>'.date("H:i").'</span> '.nick5($k,'b').' проиграла!<BR>');
							}
							mysql_query('UPDATE `users` SET `hp` = 0, `fullhptime` = '.time().', `fullmptime` = '.time().' WHERE `id` = \''.$k.'\' LIMIT 1;');
							foreach ($this->battle as $kak => $vav) {
								unset($this->battle[$kak][$k]);
							}
						 //}
						}
						if($k == null ) {
							//unset($this->battle[$k]);
							foreach ($this->battle as $kak => $vav) {
								unset($this->battle[$kak][$k]);
							}
						}
						if($us['battle'] == 0 ) {
							//unset($this->battle[$k]);
							foreach ($this->battle as $kak => $vav) {
								//unset($this->battle[$kak][$k]);
							}
						}
						unset($us);
					}
					// обновить битку
					$this->update_battle ();
				}
			}
/*-------------------------------------------------------------------
 выставляем команды, и противников/союзников
--------------------------------------------------------------------*/
			function sort_teams() {
				// режем тимзы
				$this->t1 = explode(";",$this->battle_data['t1']);
				$this->t2 = explode(";",$this->battle_data['t2']);
				// проставляем кто-где
				if (in_array ($this->user['id'],$this->t1)) {
					$this->my_class = "B1";
					$this->en_class = "B2";
					$this->team_mine = $this->t1;
					$this->team_enemy = $this->t2;
				} else {
					$this->my_class = "B2";
					$this->en_class = "B1";
					$this->team_mine = $this->t2;
					$this->team_enemy = $this->t1;
				}
			}
/*-------------------------------------------------------------------
 считаем опыт
--------------------------------------------------------------------*/
			function solve_exp ($at_id,$def_id,$damage) {
global $user;

		require_once('exp_koef.php');

echo __FILE__." ".__LINE__."<br>----<br>";			
print_r($mods);

echo "<br>----<br>".$mods["udar"];
					
					$baseexp = array(
									"0" => "5",
									"1" => "10",
									"2" => "20",
									"3" => "30",
									"4" => "60",
									"5" => "120",
									"6" => "180",
									"7" => "230",
									"8" => "350",
									"9" => "500",
									"10" => "800",
									"11" => "1500",
									"12" => "2000",
									"13" => "3000",
									"14" => "5000",
									"15" => "7000"
							);
	                if($at_id > _BOTSEPARATOR_) {
							$bots = mysql_fetch_array(mysql_query ('SELECT * FROM `bots` WHERE `id` = '.$at_id.' LIMIT 1;'));
							$at_id = $bots['prototype'];
							$bot_active = true;
					}
			        $at = mysql_fetch_array(mysql_query("SELECT * FROM `users` WHERE `id` = '".$at_id."' LIMIT 1;"));
			       	$at_cost = mysql_fetch_array(mysql_query("select 1+IFNULL((select SUM(cost)+(SUM(ecost)*10) FROM inventory WHERE owner = users.id AND dressed=1),0), `align` FROM users WHERE id = ".$at_id." LIMIT 1;"));
                    $kulak1 = mysql_fetch_array(mysql_query("select SUM(cost)+(SUM(ecost)*10) FROM inventory WHERE owner = ".$at_id." AND dressed=1 LIMIT 1;"));

                    if($def_id > _BOTSEPARATOR_) {
							$bots = mysql_fetch_array(mysql_query ('SELECT * FROM `bots` WHERE `id` = '.$def_id.' LIMIT 1;'));
							$def_id = $bots['prototype'];
							$bot_def=true;
					}
			        $def = mysql_fetch_array(mysql_query("SELECT * FROM `users` WHERE `id` = '".$def_id."' LIMIT 1;"));
			        $def_cost = mysql_fetch_array(mysql_query("select 1+IFNULL((select SUM(cost)+(SUM(ecost)*8) FROM inventory WHERE owner = users.id AND dressed=1),0), `align` FROM users WHERE id = ".$def_id." LIMIT 1;"));
                    $kulak2 = mysql_fetch_array(mysql_query("select SUM(cost)+(SUM(ecost)*10) FROM inventory WHERE owner = ".$def_id." AND dressed=1 LIMIT 1;"));

                    // модификаторы опыта
					// 100% опыта
                    //$expmf = 1;
                    // 200% опыта
                    $expmf = 2;

					//первичка
					if ($at['sergi']==0 && $at['kulon']==0 && $at['bron']==0 && $at['r1']==0 && $at['r2']==0 && $at['r3']==0 && $at['helm']==0
							&& $at['perchi']==0 && $at['boots']==0 && $at['m1']==0 && $at['m2']==0 && $at['m3']==0 && $at['m4']==0 && $at['m5']==0
							&& $at['m6']==0 && $at['m7']==0 && $at['m8']==0 && $at['m9']==0 && $at['m10']==0
							&& $at['weap']!=0 && $kulak1[0]<17){
							$expmf=$expmf*$mods['perv'];

						   if($expmf==0) 	$this->add_debug("mods['perv']=".$mods['perv']." = > ".$expmf);
						

					}

					//кулонка
					if ($at['sergi']==0 && $at['bron']==0 && $at['helm']==0
							&& $at['perchi']==0 && $at['boots']==0 && $at['m1']==0 && $at['m2']==0 && $at['m3']==0 && $at['m4']==0 && $at['m5']==0
							&& $at['m6']==0 && $at['m7']==0 && $at['m8']==0 && $at['m9']==0 && $at['m10']==0
							&& $at['weap']!=0 && $at['kulon']!=0 && $at['r1']!=0 && $at['r2']!=0 && $at['r3']!=0){
							//mfkrit,mfakrit,mfuvorot,mfauvorot
							$expmf=$expmf*$mods['kulon'];
						   if($expmf==0) 	$this->add_debug("mods['kulon']=".$mods['kulon']." = > ".$expmf);

					}

					if($this->battle_data['blood']) {//кровавые массовые
						if (($this->t1+$this->t2)>=$krov_bitv && ($this->t1+$this->t2)<$krov_rez){
							$expmf = $expmf*$mods['krov_op'];
						}
						elseif (($this->t1+$this->t2)>=$krov_rez && ($this->t1+$this->t2)<$krov_sech) $expmf = $expmf*$mods['krovr_op'];
						elseif (($this->t1+$this->t2)>=$krov_sech) $expmf = $expmf*$mods['krovs_op'];


					}
					else{ //обычные массовые
						if (($this->t1+$this->t2)>=$velikaya && ($this->t1+$this->t2)<$velichayshaya)	$expmf = $expmf*$mods['vel_op'];
						elseif (($this->t1+$this->t2)>=$velichayshaya && ($this->t1+$this->t2)<$epohalnaya) $expmf = $expmf*$mods['velich_op'];
						elseif (($this->t1+$this->t2)>=$epohalnaya) $expmf = $expmf*$mods['epoh_op'];
					}



$zv33=mysql_fetch_array(mysql_query("SELECT `prototype`,`id` FROM `bots` WHERE `prototype` = '".$user['zver_id']."' and `battle` = ".$user['battle'].""));
if($zv33){
$expmf=floor(($expmf/3)*2);
}
                    if(((int)$at['align'] == 1 && $def['align'] == 3) || ((int)$def['align'] == 1 && $at['align'] == 3)) {
                        $expmf = $expmf*$mods['alignprot'];
                    }

                    if($at['level'] > 1 && $kulak1[0]==0 && $kulak2[0]==0) {
                        $expmf = $expmf*$mods['kulakpenalty'];
                    }
                    //if($at['level'] > 1 && $at_cost[0] < $at['level']*50) {
                    //    $expmf = $expmf*0.7;
                   // } elseif($at['level'] > 1) {
                   // 	$expmf = $expmf*1.3;
                   // }
                    if($this->battle_data['blood']) {
                        $expmf = $expmf*$mods['bloodb'];
                    }
                    //$expmf = $expmf+($at_cost[0]/10000);
                        if ($this->battle_data['type']==1) {
							$btfl=fopen('tmpdisk/'.$at_id.'.btl','r');
							$contents = fread($btfl, filesize('tmpdisk/'.$at_id.'.btl'));
        	            	fclose($btfl);
            	        	$cnt=substr_count($contents,$def_id);
							$exmod=1;
                	    	if ($cnt<=1) $exmod=$mods['btl_1'];
                    		elseif ($cnt==2) $exmod=$mods['btl_2'];
                    		elseif ($cnt>2) $exmod=$mods['btl_3'];

							$expmf = $expmf*$exmod;

							// esli dralsia bolshe chem 3 raza c etim => 0
 						    if($expmf==0) 	{
								$this->add_debug("mods['exmod']=".$mods['exmod']." = > ".$expmf);
								$expmf=1; // zablokirovano poka malo ludei na starte
							}

							}

                    $standart = array(
                    				"0" => 1,
                    				"1" => 1,
                    				"2" => 15,
                    				"3" => 111,
                    				"4" => 265,
                    				"5" => 526,
                    				"6" => 882,
                    				"7" => 919,
                    				"8" => 919,
                    				"9" => 919,
                    );

                    $mfit = ($at_cost[0]/($standart[$at['level']]/3));
                    if ($mfit < 0.8) { $mfit = 0.8; }
                    if ($mfit > 1.5) { $mfit = 1.5; }

                    /*if ($bot_active == true) {
                    	$this->exp[$at_id] += ($baseexp[$def['level']])*($def_cost[0]/(($at_cost[0]+$def_cost[0])/2))*($damage/$def['maxhp'])*$expmf*$mfit*0.3;

                    }*/
                    $pls=count($this->t1)+count($this->t2);
                    if ($pls>2) {
						$mfbot= $bot_active == true ? 0.3:1;
						$mfbot2=$bot_def == true ? 0.7:1;
					}
					else { 
						$mfbot=1; 
						$mfbot2=1; 
					}

				

				if($expmf==0) $expmf=1;
if($user['room']=='403'){
				$result = (($baseexp[$def['level']])*($damage/$def['maxhp'])*$expmf*$mfit*$mfbot*$mfbot2)/12;

}else{
				$result = ($baseexp[$def['level']])*($def_cost[0]/(($at_cost[0]+$def_cost[0])/2))*($damage/$def['maxhp'])*$expmf*$mfit*$mfbot*$mfbot2;
}
 			   $debug_result = "\r\nEXP baseexp[def['level']])=".$baseexp[$def['level']]
		.") * (def_cost[0]=".$def_cost[0]."/((at_cost[0]".$at_cost[0]."+ def_cost[0]=".$def_cost[0]
		.")/2))*(damage=".$damage."/def['maxhp']=".$def['maxhp'].")* expmf=".$expmf
		." * mfit=".$mfit." * mfbot=".$mfbot."* mfbot2=".$mfbot2. " => ". $result."";
	
			$this->add_debug($debug_result);
                    if($user['align']==4) {
			$result=floor($result/2);

                    }

				//($baseexp[$def['level']])*($def_cost[0]/(($at_cost[0]+$def_cost[0])/2))*($damage/$def['maxhp'])*$expmf*$mfit*$mfbot*$mfbot2;

$result = $result/100*proc_exp;
					return $result;
			}
/*-------------------------------------------------------------------
 инициализируем размен
--------------------------------------------------------------------*/
function razmen_init ($enemy,$attack,$defend,$attack1) {
                         global $user;

	include 'incl/razmen.php';

}
/*------------------------------------------------------------------
 получаем тип оружия
--------------------------------------------------------------------*/
			function get_wep_type($idwep) {
				if ($idwep == 0 || $idwep == null || $idwep == '') {
					return "kulak";
				}
				$wep = mysql_fetch_array(mysql_query('SELECT `otdel`,`minu` FROM `inventory` WHERE `id` = '.$idwep.' LIMIT 1;'));
				if($wep[0] == '1') {
					return "noj";
				}
				elseif($wep[0] == '12') {
					return "dubina";
				}
				elseif($wep[0] == '30') {
					return "posoh";
				}
				elseif($wep[0] == '11') {
					return "topor";
				}
				elseif($wep[0] == '13') {
					return "mech";
				}
				elseif($wep[1] > 0) {
					return "buket";
				} else {
					return "kulak";
				}

			}
/*------------------------------------------------------------------
 генератор ударов =)
--------------------------------------------------------------------*/
			function razmen_log($type,$kuda,$chem,$uron,$kto,$c1,$pokomy,$c2,$hp,$maxhp) {
				$this->write_stat(nick5($kto,$c1)."|++|".nick5($pokomy,$c2)."|++|".$type."|++|".$uron."|++|".$kuda."|++|".$chem);
									//print_R(func_get_args());
					if ($this->enemyhar['sex'] && $kto == $this->enemyhar['id']) { $sex1 = false; }
					if (!$this->enemyhar['sex'] && $kto == $this->enemyhar['id']) { $sex1 = true; }
					if ($this->enemyhar['sex'] && $pokomy == $this->enemyhar['id']) { $sex2 = false; }
					if (!$this->enemyhar['sex'] && $pokomy == $this->enemyhar['id']) { $sex2 = true; }

					if ($this->user['sex'] && $kto == $this->user['id']) { $sex1 = false; }
					if (!$this->user['sex'] && $kto == $this->user['id']) { $sex1 = true; }
					if ($this->user['sex'] && $pokomy == $this->user['id']) { $sex2 = false; }
					if (!$this->user['sex'] && $pokomy == $this->user['id']) { $sex2 = true; }

					if($hp < 0) { $hp = 0; }

					// текст по промазыванию
					if (!$sex1) {
						$textfail = array ( 'думал о <вырезано цензурой>, вследствие чего',
										'пытался нанести удар, но ',
										'подскользнулся, и',
										'старался провести удар, но',
										'закашлялся, и',
										'пытался провести удар, но',
										'потерял самоконтроль, вследствие чего',
										'думал не о том, и');
					} else {
						$textfail = array ( 'думала о <вырезано цензурой>, вследствие чего',
										'пыталась нанести удар, но ',
										'подскользнулась, и',
										'старалась провести удар, но',
										'закашлялась, и',
										'пыталась провести удар, но ',
										'потеряла самоконтроль, вследствие чего',
										'думала не о том, и');
					}
					// чем били
					$textchem = array (
									"kulak" => array("грудью","ребром руки","лбом","кулаком","ногой","левой ногой","правой ногой","коленом"),
									"noj" => array("ножом","тыльной стороной лезвия ножа","рукоятью ножа","лезвием ножа"),
									"dubina" => array("сучковатой палкой","поленом","тяжелой дубиной","дубиной","рукоятью молота"),
									"posoh" => array("палкой","посохом"),
									"topor" => array("секирой","топором","лезвием секиры","алебардой","тяжелым держаком","длинной секирой"),
									"mech" => array("ножнами","гардой","мечом","лезвием меча","рукоятью меча","тупым лезвием","острой стороной меча","огромным мечом",),
									"buket" => array("охапкой цветов","веником","букетом","колючками","снопом","стеблем","листьями","бутоном",)
								);
					$textchem = $textchem[$chem];
					// куда били
					$udars = array(
						'1' => array ('в нос','в глаз','в челюсть','по переносице','в кадык','по затылку','в правый глаз','в левый глаз','в скулу'),
						'2' => array ('в грудь','в солнечное сплетение','в сердце','в бок','в область лопаток','в правое плечо','в левое плечо'),
						'3' => array ('в живот','по желудку','по левой почке','по правой почке','в пупок'),
						'4' => array ('по <вырезано цензурой>','в пах','в промежность','по левой ягодице','по правой ягодице'),
						'5' => array ('по ногам','в область правой пятки','в область левой пятки','по коленной чашечке','по икрам')
					);
					$kuda = $udars[$kuda][rand(0,count($udars[$kuda])-1)];
					//тест по попаданию
					if (!$sex1) {
						$hark = array('бесчувственный','расстроенный','храбрый','обезумевший','неустрашимый','злобный','жестокий','наглый',
										'разъяренный','продвинутый','хитрый','мужественный','','','','','','');
					}
					else {
						$hark = array('бесчувственная','расстроенная','храбрая','обезумевшая','неустрашимая','злобная','жестокая','наглая',
										'разъяренная','продвинутая','хитрая','прекрасная','','','','','','');
					}
					if (!$sex2) {
						$hark2 = array('бесчувственный','расстроенный','храбрый','обезумевший','неустрашимый','злобный','жестокий','наглый',
										'разъяренный','продвинутый','хитрый','мужественный','','','','','','');
					}
					else {
						$hark2 = array('бесчувственная','расстроенная','храбрая','обезумевшая','неустрашимая','злобная','жестокая','наглая',
										'разъяренная','продвинутая','хитрая','прекрасная','','','','','','');
					}
					if (!$sex2) {
						$textud = array ('забылся, и тут',
							'замешкался, и за это',
							'растерялся, как вдруг',
							'ковырялся в зубах, и тут',
							'поперхнулся, но вдруг',
							'пытался что-то сказать но вдруг, неожиданно',
							'растерялся, как вдруг',
							'засмотрелся на <вырезано цензурой>, а в это время',
							'высморкался, и в это время',
							'думал не о том, и',
							'пришел в себя, но в это время',
							'обернулся, как внезапно');
					} else {
						$textud = array ('забылась, и тут',
							'замешкалась, и за это ',
							'растерялась, как вдруг ',
							'ковырялась в зубах, и тут ',
							'поперхнулась, но вдруг ',
							'пыталась что-то сказать но вдруг, неожиданно',
							'растерялась, как вдруг',
							'засмотрелась на <вырезано цензурой>, а в это время',
							'высморкалась, и в это время',
							'думала не о том, и',
							'пришла в себя, но в это время ',
							'обернулась, как внезапно');
					}

				switch ($type) {
					case "mag":
						if ($sex1) {
							$textmag = "потратила";
						}
						else {
							$textmag = "потратил";
						}
						return '<span class=date>'.date("H:i").'</span> '.nick5($kto,$c1).' '.$textmag.' свой ход на магию.<BR>';
					break;
					// уворот
					case "uvorot":
						if ($sex2) {
							$textuvorot = array (" <font color=green><B>уклонилась</B></font> от удара "," <font color=green><B>увернулась</B></font> от удара "," <font color=green><B>отскочила</B></font> от удара ");
						}
						else {
							$textuvorot = array (" <font color=green><B>уклонился</B></font> от удара "," <font color=green><B>увернулся</B></font> от удара "," <font color=green><B>отскочил</B></font> от удара ");
						}
						return '<span class=date>'.date("H:i").'</span> '.nick5($kto,$c1).' '.$textfail[rand(0,count($textfail)-1)].' '.$hark2[rand(0,count($hark2)-1)].' '.nick5($pokomy,$c2).' '.$textuvorot[rand(0,count($textuvorot)-1)].' '.$textchem[rand(0,count($textchem)-1)].' '.$kuda.'.<BR>';
					break;
					//блок
					case "block":
						if ($sex2) {
							$textblock = array (" заблокировала удар "," остановила удар "," отбила удар ");
						}
						else {
							$textblock = array (" заблокировал удар "," остановил удар "," отбил удар ");
						}
						return '<span class=date>'.date("H:i").'</span> '.nick5($kto,$c1).' '.$textfail[rand(0,count($textfail)-1)].' '.$hark2[rand(0,count($hark2)-1)].' '.nick5($pokomy,$c2).' '.$textblock[rand(0,count($textblock)-1)].' '.$textchem[rand(0,count($textchem)-1)].' '.$kuda.'.<BR>';
					break;
					//крит
					case "krit":
						if ($sex1) {
							$textkrit = array (", напугав всех, неслышно подойдя сзади ударила по голове булыжником оппонента.",", сказав \"БУ!\", ласково заломила руку за спину соперника.",", расслабившись, расцарапала нос соперника.",", показав сразу два пальца, наступила на ногу врага.",", напугав всех, укусила в нос противника.",", проклиная этот сайт, провела ужасный бросок через пупок оппонента.");
						}
						else {
							$textkrit = array (", напугав всех, неслышно подойдя сзади ударил по голове булыжником оппонента.",", сказав \"БУ!\", ласково заломил руку за спину соперника.",", расслабившись, расцарапал нос соперника.",", показав сразу два пальца, наступил на ногу врага.",", напугав всех, укусил в нос противника.",", проклиная этот сайт, провел ужасный бросок через пупок оппонента.");
						}
						return '<span class=date>'.date("H:i").'</span> '.nick5($pokomy,$c2).' '.$textud[rand(0,count($textud)-1)].' '.$hark[rand(0,count($hark)-1)].' '.nick5($kto,$c1).' '.$textkrit[rand(0,count($textkrit)-1)].' <b><font color=red>-'.$uron.'</font></b> ['.$hp.'/'.$maxhp.']'.'<BR>';
					break;
					//крит
					case "krita":
						if ($sex1) {
							$textkrit = array (", напугав всех, неслышно подойдя сзади ударила, пробив блок, по голове булыжником оппонента.",",  пробив блок, ласково заломила руку за спину соперника.",", пробив блок, расцарапала нос соперника.",", пробив блок, наступила на ногу врага.",", пробив блок, укусила в нос противника.",", пробив блок, провела ужасный бросок через пупок оппонента.");
						}
						else {
							$textkrit = array (", напугав всех, неслышно подойдя сзади ударил, пробив блок, по голове булыжником оппонента.",", пробив блок, ласково заломил руку за спину соперника.",", пробив блок, расцарапал нос соперника.",", пробив блок, наступил на ногу врага.",", пробив блок, укусил в нос противника.",", пробив блок, провел ужасный бросок через пупок оппонента.");
						}
						return '<span class=date>'.date("H:i").'</span> '.nick5($pokomy,$c2).' '.$textud[rand(0,count($textud)-1)].' '.$hark[rand(0,count($hark)-1)].' '.nick5($kto,$c1).' '.$textkrit[rand(0,count($textkrit)-1)].' <b><font color=red>-'.$uron.'</font></b> ['.$hp.'/'.$maxhp.']'.'<BR>';
					break;
					// попадание
					case "udar":
						if ($sex1) {
							$textudar = array(", разбежавшись, рубанула"," отчаянно проткнула "," нехотя уколола "," не подумав, рубанула ",", улыбаясь, саданула укол "," приложила удар "," ударила "," сдуру вмазала ");
						}
						else {
							$textudar = array(", разбежавшись, рубанул"," отчаянно проткнул "," нехотя уколол "," не подумав, рубанул ",", улыбаясь, саданул укол "," приложил удар "," ударил "," сдуру вмазал ");
						}
						return '<span class=date>'.date("H:i").'</span> '.nick5($pokomy,$c2).' '.$textud[rand(0,count($textud)-1)].' '.$hark[rand(0,count($hark)-1)].' '.nick5($kto,$c1).''.$textudar[rand(0,count($textudar)-1)].' '.$textchem[rand(0,count($textchem)-1)].' '.$kuda.' <b>-'.$uron.'</b> ['.$hp.'/'.$maxhp.']'.'<BR>';
					break;
				}
			}
/*------------------------------------------------------------------
 проверка на попадание "куда надо"
--------------------------------------------------------------------*/
			function get_block ($komy,$att,$def,$enemy) {
				//  по типам блоков
if($komy=="me"){$kogochekat=$this->user['id'];}elseif($komy=="he"){$kogochekat=$enemy;}

if(mysql_fetch_array(mysql_query('SELECT id FROM `inventory` WHERE `type` = 10 AND `dressed` = 1 AND `owner` = '.$kogochekat.';'))){
						$blocks = array (
							'1' => array (1,2,3),
							'2' => array (2,3,4),
							'3' => array (3,4,5),
							'4' => array (4,5,1),
							'5' => array (5,1,2)
						);
}else{
						$blocks = array (
							'1' => array (1,2),
							'2' => array (2,3),
							'3' => array (3,4),
							'4' => array (4,5),
							'5' => array (5,1)
						);
}
/*				$this->write_stat_block(nick5($this->user['id'],$this->my_class)."|++|".implode('/',$blocks[$def]));
				$this->write_stat_block(nick5($enemy,$this->en_class)."|++|".implode('/',$blocks[$this->battle[$enemy][$this->user['id']][1]]));*/
				switch ($komy) {
					case "me" :
						if (!in_array($this->battle[$enemy][$this->user['id']][0],$blocks[$def])) {
							return true;
						} else {
							return false;
						}
					break;
					// бьем вражину
					case "he" :
						if (!in_array($att,$blocks[$this->battle[$enemy][$this->user['id']][1]])) {
							return true;
						} else {
							return false;
						}
					break;
				}
			}
/*------------------------------------------------------------------
 возвращает получился шанс или нет
--------------------------------------------------------------------*/
			function get_chanse ($persent) {
				//srand(microtime());
				$mm=1000000;
				if (rand($mm,100*$mm) <= $persent*$mm) {
					return true;
				}
				else {
					return false;
				}
			}
/*------------------------------------------------------------------
 выбираем противничка
--------------------------------------------------------------------*/
			function select_enemy() {
				if(($this->user['hp']>0) && $this->battle) {
					foreach($this->battle[$this->user['id']] as $k => $v) {
						if ($this->battle[$this->user['id']][$k][0] == 0) {
							$enemys[] = $k;
						}
					}
					return $enemys[rand(0,count($enemys)-1)];
				} else {
					return 0;
				}
			}
/*------------------------------------------------------------------
 считаем модификаторы
--------------------------------------------------------------------*/
			function solve_mf($enemy,$myattack,$myattack1) {

        //БОНУСЫ!
        $zo=mysql_fetch_row(mysql_query("SELECT id FROM effects WHERE type=201 AND owner=".(int)$this->user['id']." LIMIT 1;"));
        $zo1=mysql_fetch_row(mysql_query("SELECT id FROM effects WHERE type=201 AND owner=".(int)$this->enemyhar['id']." LIMIT 1;"));
        $sokr=mysql_fetch_row(mysql_query("SELECT id FROM effects WHERE type=202 AND owner=".(int)$this->user['id']." LIMIT 1;"));
        $sokr1=mysql_fetch_row(mysql_query("SELECT id FROM effects WHERE type=202 AND owner=".(int)$this->enemyhar['id']." LIMIT 1;"));
//        $bmfud= $sokr[0]>0 ? 2:0;//владение оружием ! закомментено, потому что переделано ниже
//        $bmfbron= $zo[0]>0 ? 4:0;//броня закомментено, потому что переделано ниже
          $bmfbron=0; $bmfud=0; $bmfud1=0; $bmfbron1=0;
        $bmfuv=0; $bmfauv=0; $bmfakrit=0; $bmfkrit=0; //модификаторы
 //       $bmfud1= $sokr1[0]>0 ? 2:0;//владение оружием закомментено, потому что переделано ниже
//        $bmfbron1= $zo1[0]>0 ? 4:0;//броня закомментено, потому что переделано ниже
        $bmfuv1=0; $bmfauv1=0; $bmfakrit1=0; $bmfkrit1=0; //модификаторы
        if ($this->user['sila']>=25) $bmfud+=1;
        if ($this->user['sila']>=50) $bmfud+=2;
        if ($this->user['lovk']>=25) $bmfauv+=25;
        if ($this->user['lovk']>=50) $bmfuv+=25;
        if ($this->user['inta']>=25) $bmfakrit+=25;
        if ($this->user['inta']>=50) $bmfkrit+=25;
        if ($this->user['vinos']>=25) $bmfbron+=2;
        if ($this->user['vinos']>=50) $bmfbron+=4;

        if ($this->enemyhar['sila']>=25) $bmfud1+=1;
        if ($this->enemyhar['sila']>=50) $bmfud1+=2;
        if ($this->enemyhar['lovk']>=25) $bmfauv1+=25;
        if ($this->enemyhar['lovk']>=50) $bmfuv1+=25;
        if ($this->enemyhar['inta']>=25) $bmfakrit1+=25;
        if ($this->enemyhar['inta']>=50) $bmfkrit1+=25;
        if ($this->enemyhar['vinos']>=25) $bmfbron1+=2;
        if ($this->enemyhar['vinos']>=50) $bmfbron1+=4;

        //*************

				$mf = array ();
				if($enemy > _BOTSEPARATOR_) {
					$bots = mysql_fetch_array(mysql_query ('SELECT * FROM `bots` WHERE `id` = '.$enemy.' LIMIT 1;'));
					$this->enemyhar = mysql_fetch_array(mysql_query('SELECT * FROM `users` WHERE `id` = \''.$bots['prototype'].'\' LIMIT 1;'));
					$this->enemy_dress = mysql_fetch_array(mysql_query('SELECT sum(minu),sum(maxu),sum(mfkrit),sum(mfakrit),sum(mfuvorot),sum(mfauvorot),sum(bron1),sum(bron2),sum(bron2),sum(bron3),sum(bron4),sum(mfkritpow) FROM `inventory` WHERE `dressed`=1 AND `owner` = \''.$bots['prototype'].'\' LIMIT 1;'));
					$this->enemyhar['hp'] = $bots['hp'];
				} else {
					$this->enemyhar = mysql_fetch_array(mysql_query('SELECT * FROM `users` WHERE `id` = \''.$enemy.'\' LIMIT 1;'));
					$this->enemy_dress = mysql_fetch_array(mysql_query('SELECT sum(minu),sum(maxu),sum(mfkrit),sum(mfakrit),sum(mfuvorot),sum(mfauvorot),sum(bron1),sum(bron2),sum(bron2),sum(bron3),sum(bron4),sum(mfkritpow) FROM `inventory` WHERE `dressed`=1 AND `owner` = \''.$enemy.'\' LIMIT 1;'));
				}
				$this->user_dress = mysql_fetch_array( mysql_query('SELECT sum(minu),sum(maxu),sum(mfkrit),sum(mfakrit),sum(mfuvorot),sum(mfauvorot),sum(bron1),sum(bron2),sum(bron2),sum(bron3),sum(bron4),sum(mfkritpow) FROM `inventory` WHERE `dressed`=1 AND `owner` = \''.$this->user['id'].'\' LIMIT 1;'));
				$this->user_u1 = mysql_fetch_array( mysql_query('SELECT minu,maxu FROM `inventory` WHERE `id` = \''.$this->user['weap'].'\' LIMIT 1;'));
				$this->user_u2 = mysql_fetch_array( mysql_query('SELECT minu,maxu FROM `inventory` WHERE `id` = \''.$this->user['shit'].'\' AND `second`=1 LIMIT 1;'));


$this->user_dress[12]=floor($this->user_dress[0]-$this->user_u1[0]);
$this->user_dress[13]=floor($this->user_dress[1]-$this->user_u1[1]);
$this->user_dress[0]=floor($this->user_dress[0]-$this->user_u2[0]);
$this->user_dress[1]=floor($this->user_dress[1]-$this->user_u2[1]);


$chpercbron1=floor($bmfbron1/100*30);
        $bmfbron1 += $zo1[0]>0 ? $chpercbron1:0;//свиток защиты !

        $this->enemy_dress[6]+=$bmfbron1;
        $this->enemy_dress[7]+=$bmfbron1;
        $this->enemy_dress[8]+=$bmfbron1;
        $this->enemy_dress[9]+=$bmfbron1;
        $this->enemy_dress[10]+=$bmfbron1;


$chpercbron=floor($bmfbron/100*30);
        $bmfbron += $zo[0]>0 ? $chpercbron:0;//свиток защиты !

        $this->user_dress[6]+=$bmfbron;
        $this->user_dress[7]+=$bmfbron;
        $this->user_dress[8]+=$bmfbron;
        $this->user_dress[9]+=$bmfbron;
        $this->user_dress[10]+=$bmfbron;

				//print_r($this->user);
				//print_r($this->enemyhar);

				// мои МФ в отношении противника
				//mt_srand(microtime());


				$mykrit = $this->user_dress[2]+$this->user['inta']*2.95+$bmfkrit;
				//if($mykrit < 1) { $mykrit = 1; } elseif ($mykrit > 50) { $mykrit = 50; }
				$heakrit = $this->enemy_dress[3]+$this->enemyhar['inta']*2.75+$this->enemyhar['lovk']*0+$bmfakrit1;
				//if($heakrit < 1) { $heakrit = 1; } elseif ($heakrit > 50) { $heakrit = 50; }
				$myuvorot = $this->user_dress[4]+$this->user['lovk']*5+$this->user['inta']*0+$bmfuv;
				//if($myuvorot < 1) { $myuvorot = 1; } elseif ($myuvorot > 50) { $myuvorot = 50; }
				$heauvorot = $this->enemy_dress[5]+$this->enemyhar['lovk']*4+$this->enemyhar['inta']*1.35+$bmfauv1;
				//if($heauvorot < 1) { $heauvorot = 1; } elseif ($heauvorot > 50) { $heauvorot = 50; }
/*				exec("echo ========= >> blog.log");
				exec("echo \"[".$this->user['id']."]:S".$this->user['sila'].":L".$this->user['lovk'].":I".$this->user['inta'].":V".$this->user['vinos'].":\" >> blog.log");
				exec("echo \"[".$this->user['id']."]:DU".$this->user_dress['4'].":DAU".$this->user_dress['5'].":DK".$this->user_dress['2'].":DAK".$this->user_dress['3'].":\" >> blog.log");
				exec("echo \"[".$this->enemyhar['id']."]:S".$this->enemyhar['sila'].":L".$this->enemyhar['lovk'].":I".$this->enemyhar['inta'].":V".$this->enemyhar['vinos'].":\" >> blog.log");
				exec("echo \"[".$this->enemyhar['id']."]:DU".$this->enemy_dress['4'].":DAU".$this->enemy_dress['5'].":DK".$this->enemy_dress['2'].":DAK".$this->enemy_dress['3'].":\" >> blog.log");
*/

				$mf['me'] = array (
									'udar' => rand(floor($this->user['sila']/3)+1+$this->user_dress[0],floor($this->user['sila']/3)+4+$this->user_dress[1])-rand(0,$this->enemy_dress[5+$myattack]),
                                                                        'udar1' => rand(floor($this->user['sila']/3)+1+$this->user_dress[12],floor($this->user['sila']/3)+4+$this->user_dress[13])-rand(0,$this->enemy_dress[5+$myattack1]),
									//'krit' => (0-$this->enemy_dress[3]-floor($this->enemyhar['inta']*4)+$this->user_dress[2]+floor($this->user['inta']*4)),
									//'uvorot' => (0-$this->enemy_dress[5]-floor($this->enemyhar['lovk']*4)+$this->user_dress[4]+floor($this->user['lovk']*4)),
									'krit' => $mykrit-$heakrit, //(1-($heakrit+70)/($mykrit+70))*70,	//(1 - $heakrit/$mykrit)*100, //
									'uvorot' => $myuvorot-$heauvorot, //(1-($heauvorot+80)/($myuvorot+80))*53, //(1 - $heauvorot/$myuvorot)*0.8*100, //
							);
					if($mf['me']['udar'] < 0) { $mf['me']['udar'] = 1; } elseif ($mf['me']['udar'] < 1) { $mf['me']['udar'] = rand(1,15); }
					if($mf['me']['udar1'] < 0) { $mf['me']['udar1'] = 1; } elseif ($mf['me']['udar1'] < 1) { $mf['me']['udar1'] = rand(1,15); }
					if($mf['me']['krit'] < 1) { $mf['me']['krit'] = 1; } elseif ($mf['me']['krit'] > 50) { $mf['me']['krit'] = 50; }
					if($mf['me']['uvorot'] < 1) { $mf['me']['uvorot'] = 1; } elseif ($mf['me']['uvorot'] > 65) { $mf['me']['uvorot'] = 65; }
					if($this->get_wep_type($this->user['weap']) == 'kulak' && $this->user['align'] == '2') { $mf['me']['udar'] += $this->user['level'];}
					switch($this->get_wep_type($this->user['weap'])) {
						case "noj":
							$mf['me']['udar'] += $this->user['noj'];
							$mf['me']['udar1'] += $this->user['noj'];
						break;
						case "dubina":
							$mf['me']['udar'] += $this->user['dubina'];
							$mf['me']['udar1'] += $this->user['dubina'];
						break;
						case "posoh":
							$mf['me']['udar'] += $this->user['posoh'];
							$mf['me']['udar1'] += $this->user['posoh'];
						break;
						case "topor":
							$mf['me']['udar'] += $this->user['topor'];
							$mf['me']['udar1'] += $this->user['topor'];
						break;
						case "mech":
							$mf['me']['udar'] += $this->user['mec'];
							$mf['me']['udar1'] += $this->user['mec'];
						break;
					}

          $mf['me']['udar']+=$bmfud;
          $mf['me']['udar1']+=$bmfud;
$chpercud=floor($mf['me']['udar']/100*25);
        $sokrud = $sokr[0]>0 ? $chpercud:0;//сокруха !
$chpercud2=floor($mf['me']['udar1']/100*25);
        $sokrud2 = $sokr[0]>0 ? $chpercud2:0;//сокруха !
          $mf['me']['udar']+=$sokrud;
          $mf['me']['udar1']+=$sokrud2;

				// МФ врага в отношении меня
				mt_srand(microtime());

				$myakrit = $this->user_dress[3]+$this->user['inta']*2.75+$this->user['lovk']*0+$bmfakrit;
				//if($myakrit < 1) { $myakrit = 1; } elseif ($myakrit > 50) { $myakrit = 50; }
				$hekrit = $this->enemy_dress[2]+$this->enemyhar['inta']*2.95+$bmfkrit1;
				//if($hekrit < 1) { $hekrit = 1; } elseif ($hekrit > 50) { $hekrit = 50; }
				$myauvorot = $this->user_dress[5]+$this->user['lovk']*4+$this->user['inta']*1.35+$bmfauv;
				//if($myauvorot < 1) { $myauvorot = 1; } elseif ($myauvorot > 50) { $myauvorot = 50; }
				$heuvorot = $this->enemy_dress[4]+$this->enemyhar['lovk']*5+$this->enemyhar['inta']*0+$bmfuv1;
				//if($heuvorot < 1) { $heuvorot = 1; } elseif ($heuvorot > 50) { $heuvorot = 50; }

/*				exec("echo \"[".$this->user['id']."]:SVU".$myuvorot.":SVAU".$myauvorot.":SVK".$mykrit.":SVAK".$myakrit.":\" >> blog.log");
				exec("echo \"[".$this->enemyhar['id']."]:SVU".$heuvorot.":SVAU".$heauvorot.":SVK".$hekrit.":SVAK".$heakrit.":\" >> blog.log");*/


//print_r($mf['me']);


				$mf['he'] = array (
									'udar' => rand(floor($this->enemyhar['sila']/3)+1+$this->enemy_dress[0],floor($this->enemyhar['sila']/3)+4+$this->enemy_dress[1])-rand(0,$this->user_dress[5+$this->battle[$enemy][$this->user['id']][0]]),
									'udar1' => rand(floor($this->enemyhar['sila']/3)+1+$this->enemy_dress[0],floor($this->enemyhar['sila']/3)+4+$this->enemy_dress[1])-rand(0,$this->user_dress[5+$this->battle[$enemy][$this->user['id']][0]]),
									'krit' =>	$hekrit-$myakrit, //(1-($myakrit+70)/($hekrit+70))*70, //(1 - $myakrit/$hekrit)*100, //
									'uvorot' => $heuvorot-$myauvorot, //(1-($myauvorot+80)/($heuvorot+80))*53, //(1 - $myauvorot/$heuvorot)*0.8*100, //
							);		
					if($mf['he']['udar'] < 0) { $mf['he']['udar'] = 1; } elseif ($mf['he']['udar'] < 1 && !$enemy > _BOTSEPARATOR_) { $mf['he']['udar'] = rand(1,15); } elseif ($mf['he']['udar'] == 0) { $mf['he']['udar'] = rand(1,3); }
					if($mf['he']['udar1'] < 0) { $mf['he']['udar1'] = 1; } elseif ($mf['he']['udar1'] < 1 && !$enemy > _BOTSEPARATOR_) { $mf['he']['udar1'] = rand(1,15); } elseif ($mf['he']['udar1'] == 0) { $mf['he']['udar1'] = rand(1,3); }
					if($mf['he']['krit'] < 1) { $mf['he']['krit'] = 1; } elseif ($mf['he']['krit'] > 50) { $mf['he']['krit'] = 50; }
					if($mf['he']['uvorot'] < 1) { $mf['he']['uvorot'] = 1; } elseif ($mf['he']['uvorot'] > 65) { $mf['he']['uvorot'] = 65; }
					if($this->get_wep_type($this->enemyhar['weap']) == 'kulak' && $this->enemyhar['align'] == '2') { $mf['he']['udar'] += $this->enemyhar['level']; }
					switch($this->get_wep_type($this->enemyhar['weap'])) {
						case "noj":
							$mf['he']['udar'] += $this->enemyhar['noj'];
							$mf['he']['udar1'] += $this->enemyhar['noj'];
						break;
						case "dubina":
							$mf['he']['udar'] += $this->enemyhar['dubina'];
							$mf['he']['udar1'] += $this->enemyhar['dubina'];
						break;
						case "posoh":
							$mf['he']['udar'] += $this->enemyhar['posoh'];
							$mf['he']['udar1'] += $this->enemyhar['posoh'];
						break;
						case "topor":
							$mf['he']['udar'] += $this->enemyhar['topor'];
							$mf['he']['udar1'] += $this->enemyhar['topor'];
						break;
						case "mech":
							$mf['he']['udar'] += $this->enemyhar['mec'];
							$mf['he']['udar1'] += $this->enemyhar['mec'];
						break;
					}
          $mf['he']['udar']+=$bmfud1;
          $mf['he']['udar1']+=$bmfud1;
$chpercud1=floor($mf['he']['udar']/100*25);
        $sokrud1 = $sokr1[0]>0 ? $chpercud1:0;//сокруха !
$chpercud12=floor($mf['he']['udar']/100*25);
        $sokrud12 = $sokr1[0]>0 ? $chpercud12:0;//сокруха !
          $mf['he']['udar']+=$sokrud1;
          $mf['he']['udar1']+=$sokrud12;

					//if(in_array($enemy,$this->botsid)) { $mf['he']['udar'] += $this->user_dress[1]; }
					if($enemy > _BOTSEPARATOR_) {
						$mf['he']['krit'] -= 6;
					}
				// result
//				print_r($mf['he']);
//$mf['he']['udar']=floor($mf['he']['udar']/0.75);
//$mf['me']['udar']=floor($mf['me']['udar']/0.75);
				return $mf;
			}

/*------------------------------------------------------------------
 сейвим бой
--------------------------------------------------------------------*/
			function update_battle () {
				return mysql_query('UPDATE `battle` SET `exp` = \''.serialize($this->exp).'\', `teams` = \''.serialize($this->battle).'\', `damage` = \''.serialize($this->damage).'\' WHERE `id` = '.$this->battle_data['id'].' ;');
			}
/*------------------------------------------------------------------
 генератор фраз комментатора
--------------------------------------------------------------------*/
			function get_comment () {
				$boycom = array (
					'А танцуешь ты лучше.',
'А мы что, в прятки тут играем?',
'А вы разве пингвинов никогда не видели?',
'А, ведь когда-то, вы были красивыми… А теперь? Ну и рожи! Жуть!',
'А потом еще труп пинать будут.',
'А я вчера ночью за соседями подглядывал. Они точно так же кувыркались',
'А ведь вы живых людей дубасите...',
'А вот я вчера в зоопарке был...',
'А вы в стройбате не служили?',
'А вы видели, чтобы так на улице делали!?',
'А вы знали что ёжики размножаются в интернете?',
'А жить-то, как хочется:',
'А из-за чего вы собственно дерётесь?',
'А чего ржёте, вы ещё остальных не видели',
'А что произойдёт если ты испугаешся до полусмерти дважды?!',
'Больше так не делай. Ты же не садист?',
'Без комментариев...',
'Больно ведь!',
'Быстро ты за монитор спрятался!',
'Все хотят попасть в рай, но никто не хочет умирать!',
'Вчера с такой девчонкой познакомился.',
'Всего 5 минут знакомы, а дерутся, словно супруги с 20-ти летним стажем...',
'Все. Я так больше не могу.',
'В конце концов, кто-то победит?',
'Вы чего, с дерева упали? ',
'Возятся как сонные мухи... давайте я вам лучше анекдот расскажу: ...',
'Вот видишь, как полезно чистить зубы на ночь?',
'Вот вы все руками махаете, а за вами уже очередь',
'Вот попадёте вы в плен и вас там будут долго бить. Но вы ничего не расскажете... и не потому, что вы такой стойкий, просто вы ничего не знаете',
'Вы бы лучше пошли потренировались!',
'Вы все еще разминаетесь? Позовите, когда кости в муку друг другу разминать будете.',
'Вы же бойцы! Имейте совесть!',
'Гаси недоумка!',
'Да, если бы я смог это остановить, то получил бы нобелевскую премию "За мир" ',
'Да куда они бьют?!',
'Давайте быстрее! За вами уже очередь образовалась.',
'Давайте обойдемся сегодня таймаутом. А? А то мне уже кошмары скоро будут сниться.',
'Дерутся как девчонки!',
'Дети, посмотрите налево... Ой!.. Нет, туда лучше не смотреть.',
'Если так будет продолжаться, то скоро мы заснем!',
'Если бы у меня было кресло-качалка, я бы в нём качался...',
'Если вы что-то сказать хотите, то лучше молчите :)',
'Жестокость не порок.',
'Жизнь вне нашего клуба - это пустая трата кислорода!!!',
'Жми! Дави! Кусай! Царапай!',
'За такие бои надо в хаос отправлять!',
'Знаете откуда в комиссионном магазине столько вещей? Это я после ваших гулянок собираю и сдаю туда. Иногда вместе с частями тела, застрявшими в них.',
'Здесь люди так близки друг к другу. Просто иначе ударить нельзя.',
'И пролитая кровь еще пульсирует...',
'Инвалидов развелось...',
'Какой бой!!!',
'Какой боец, такой конец',
'Кто!? Кто здесь?!',
'Кто вас этому научил?',
'Кузнечик, блин...',
'Куплю импортный проигрыватель грампластинок.',
'Лошадью ходи!',
'Лучше враг, чем друг - враг.',
'Ладно, вы тут пока друг друга за волосы таскайте, а я пойду, пообедаю.',
'Мне ваш балет уже надоел!',
'Может, начнется-таки настоящий бой???',
'Мысли лезут в голову изнутри, а удары снаружи.',
'Ну и где ваши коронные удары? Где живописные падения я спрашиваю!',
'Ну, нельзя же так наотмашь лупить!',
'Надо раньше было думать, теперь смертельно поздно...',
'На такое зрелище билеты продавать можно. Народ ухохочется!',
'Нет! Не надо драки! А... ладно деритесь, все равно не умеете.',
'Нет, ну должен быть повод, должен же быть повод?',
'Нет, я отказываюсь это комментировать!',
'Не таких обламывали!',
'Ну выпили вы рюмку, ну две... ну литр, ну два... так зачем же после этого драку затевать?!',
'Ну и кто за этот погром платить будет?',
'Ну и оскал у вас. Из вашей улыбки кастеты делать можно.',
'Ну, что же ты..? Не печалься. Выше голову, так по ней удобней попасть.',
'Ничего... Блок тоже удар.',
'Обернись!!!.... Поздно...',
'Ого! Научите меня так не делать.',
'Осторожно! Сделаешь дырочку, уже не запломбируешь!',
'Оно вам надо???',
'Обычное дело...там что-то отклеилось.',
'Ой, и заболтался я с вами...',
'Он же не промахнётся если ты не отойдёшь!',
'По-моему, кому-то светит инвалидность.',
'Подкинь ему грабли, на которые он еще не наступал.',
'Прав был кот Леопольд, давайте жить дружно?',
'При ударе в живот нарушается кислотно-щелочной баланс.',
'Проверь, не торчит ли у тебя нож из живота.',
'Перестаньте мне орать!',
'Подкинь ему грабли, на которые он еще не наступал.',
'Прыгают тут как блохи... Все, я пошел за дихлофосом!',
'Разбудите меня когда эта порнография закончится...',
'Ребенок сильнее ударил бы!',
'Славно вмазал!',
'Славно они веселятся',
'Смотрю вот на вас, и слезы наворачиваются.',
'Сначала учатся ходить, а потом только в драку лезут.',
'Так они друг другу что-нибудь сломают.',
'Так ты ему все кости переломаешь!',
'У меня в подъезде точно так же соседа отмудохали',
'Убогих развелось...',
'Ух ты, какой прыткий!',
'Фашист!! Надо ж, так по больному месту врезать...',
'Хватит бить его об угол моей кабинки! Мне же потом ее чинить.',
'Хулиганы, прекратите немедленно!',
'Хочешь, подскажу, куда он ударит?',
'Хорошо, что у меня ловкости больше чем у вас всех, а то б вы и меня в инвалидную коляску посадили бы.',
'Хороший бой!',
'Хороший удар!',
'Хиляк-разрядник!',
'Что ты его за волосы схватил?! Отпусти немедленно!',
'Щас я вас настигну, вот тогда мы и похохочем',
'Это была какая-то неизвестная мне техника...',
'Это же противник, а не глина! Хватит мяться!',
'Это не бой, это издевательское избиение.',
'Это поубавит спеси',
'Это и был твой план "Б" ?',
'Это была какая-то неизвестная мне техника...',
'Я же предупреждал, - будет больно.',
'Я не страдаю безумием. Я наслаждаюсь им каждую минуту :)',
'Я красивый, я сильный, я умный, я добрый. А вот вы? Вы себя-то видели?!',
'Я тоже умею драться, но не буду...',
'(тревожно озираясь) я вам по секрету скажу... за вами наблюдают!',
'<вырезано цензурой> после боя я этих <вырезано цензурой> обоих в <вырезано цензурой> и <вырезано цензурой>',
'<вырезано цензурой> каратисты фиговы');

				// рандомайзим и шансуем ;)
				if (rand(0,3)==1) {
					return '<span class=date>'.date("H:i").'</span> <i>Комментатор: '.$boycom[rand(0,count($boycom)-1)].'</i><BR>';
				}
				else {
					return false;
				}
			}
/*------------------------------------------------------------------
 есть ли тайм в советском союзе?
--------------------------------------------------------------------*/
			function get_timeout () {
				if($this->battle) {
					if ($this->my_class=='B1') {
						if($this->to2 <= $this->to1) {
							return ((time()-$this->to2) > $this->battle_data['timeout']*60);
						} else {
							return false;
						}
					} else {
						if($this->to2 >= $this->to1) {
							return ((time()-$this->to1) > $this->battle_data['timeout']*60);
						} else {
							return false;
						}
					}
				}
			}
/*-------------------------------------------------------------------
  работа с логами
--------------------------------------------------------------------*/
			function add_log ($text) {

                        $this->log .= $text;
			}

			function write_log () {
if($this->log){$this->log=$this->log."<hr>";}


				//mysql_query('UPDATE `logs` SET `log` = CONCAT(`log`,\''.$this->log.'\') WHERE `id` = '.$this->user['battle'].'');

				$fp = fopen ("backup/logs/battle".$this->user['battle'].".txt","a"); //открытие
				flock ($fp,LOCK_EX); //БЛОКИРОВКА ФАЙЛА
				fputs($fp , $this->log); //работа с файлом
				fflush ($fp); //ОЧИЩЕНИЕ ФАЙЛОВОГО БУФЕРА И ЗАПИСЬ В ФАЙЛ
				flock ($fp,LOCK_UN); //СНЯТИЕ БЛОКИРОВКИ
				fclose ($fp); //закрытие
				$this->log = '';
			}

			function write_stat ($text) {
				$fp = fopen ("backup/stat/battle".$this->user['battle'].".txt","a"); //открытие
				flock ($fp,LOCK_EX); //БЛОКИРОВКА ФАЙЛА
				fputs($fp , $text."\n"); //работа с файлом
				fflush ($fp); //ОЧИЩЕНИЕ ФАЙЛОВОГО БУФЕРА И ЗАПИСЬ В ФАЙЛ
				flock ($fp,LOCK_UN); //СНЯТИЕ БЛОКИРОВКИ
				fclose ($fp); //закрытие
			}
			function write_stat_block ($text) {
				$fp = fopen ("backup/stat/battle_block".$this->user['battle'].".txt","a"); //открытие
				flock ($fp,LOCK_EX); //БЛОКИРОВКА ФАЙЛА
				fputs($fp , $text."\n"); //работа с файлом
				fflush ($fp); //ОЧИЩЕНИЕ ФАЙЛОВОГО БУФЕРА И ЗАПИСЬ В ФАЙЛ
				flock ($fp,LOCK_UN); //СНЯТИЕ БЛОКИРОВКИ
				fclose ($fp); //закрытие
			}

			function add_debug ($text) {
				$this->log_debug .= $text;
			}

			function write_debug () {
				//mysql_query('UPDATE `logs` SET `log` = CONCAT(`log`,\''.$this->log.'\') WHERE `id` = '.$this->user['battle'].'');
echo $this->log_debug;

				$fp = fopen ("backup/debug/battle".$this->user['battle'].".txt","a"); //открытие
				flock ($fp,LOCK_EX); //БЛОКИРОВКА ФАЙЛА
				fputs($fp , $this->log_debug) ; //работа с файлом
				fflush ($fp); //ОЧИЩЕНИЕ ФАЙЛОВОГО БУФЕРА И ЗАПИСЬ В ФАЙЛ
				flock ($fp,LOCK_UN); //СНЯТИЕ БЛОКИРОВКИ
				fclose ($fp); //закрытие
				$this->log_debug = '';

//die();
			}


	}

// ========================================================================================================================================================
// конец исполняемого кода битвы
//=========================================================================================================================================================
	$fbattle = new fbattle($user['battle']);

?>
<HTML>
<HEAD>
<link rel=stylesheet type="text/css" href="i/main.css">
<meta content="text/html; charset=windows-1251" http-equiv=Content-type>
<META Http-Equiv=Cache-Control Content=no-cache>
<meta http-equiv=PRAGMA content=NO-CACHE>
<META Http-Equiv=Expires Content=0>
<SCRIPT>
<?
if(mysql_fetch_array(mysql_query('SELECT id FROM `inventory` WHERE `type` = 10 AND `dressed` = 1 AND `owner` = '.$user['id'].';'))){?>
var def_zones = 'головы, груди и живота;груди, живота и пояса;живота, пояса и ног;пояса, ног и головы;ног, головы и груди'.split(';');
<?}else{?>
var def_zones = 'головы и груди,груди и живота,живота и пояса,пояса и ног,ног и головы'.split(',');
<?}

if($sumwear==2){?>
var attacks =  2;   // кол. атак
<?}else{?>
var attacks =  1;   // кол. атак
<?}?>

</SCRIPT>
<SCRIPT src='i/combats_096_ru.js'></SCRIPT>
<SCRIPT LANGUAGE="JavaScript" SRC="i/sl2.js"></SCRIPT>
<SCRIPT LANGUAGE="JavaScript" SRC="i/ch.js"></SCRIPT>

<SCRIPT>

function fullfastshow(a,b,c,d,e,f){if(typeof b=="string")b=a.getElementById(b);var g=c.srcElement?c.srcElement:c,h=g;c=g.offsetLeft;for(var j=g.offsetTop;h.offsetParent&&h.offsetParent!=a.body;){h=h.offsetParent;c+=h.offsetLeft;j+=h.offsetTop;if(h.scrollTop)j-=h.scrollTop;if(h.scrollLeft)c-=h.scrollLeft}if(d!=""&&b.style.visibility!="visible"){b.innerHTML="<small>"+d+"</small>";if(e){b.style.width=e;b.whiteSpace=""}else{b.whiteSpace="nowrap";b.style.width="auto"}if(f)b.style.height=f}d=c+g.offsetWidth+10;e=j+5;if(d+b.offsetWidth+3>a.body.clientWidth+a.body.scrollLeft){d=c-b.offsetWidth-5;if(d<0)d=0}if(e+b.offsetHeight+3>a.body.clientHeight+a.body.scrollTop){e=a.body.clientHeight+ +a.body.scrollTop-b.offsetHeight-3;if(e<0)e=0}b.style.left=d+"px";b.style.top=e+"px";if(b.style.visibility!="visible")b.style.visibility="visible"}function fullhideshow(a){if(typeof a=="string")a=document.getElementById(a);a.style.visibility="hidden";a.style.left=a.style.top="-9999px"}

function gfastshow(dsc, dx, dy) { fullfastshow(document, mmoves3, window.event, dsc, dx, dy); }
function ghideshow() { fullhideshow(mmoves3); }





function Prv(logins)
{
	top.frames['bottom'].window.document.F1.text.focus();
	top.frames['bottom'].document.forms[0].text.value = logins + top.frames['bottom'].document.forms[0].text.value;
}
function setattack() {attack=true}
function setdefend() {defend=true}
</SCRIPT>
<script>
			function refreshPeriodic()
			{
				<?if($fbattle->battle) {	?>location.href='<?=$_SERVER['PHP_SELF']?>?batl=<?=$_REQUEST['batl']?>';//reload();
				<?}?>
				timerID=setTimeout("refreshPeriodic()",30000);
			}
			timerID=setTimeout("refreshPeriodic()",30000);
</script>
<style type="text/css">
.menu {
  background-color: #d2d0d0;
  border-color: #ffffff #626060 #626060 #ffffff;
  border-style: solid;
  border-width: 1px;
  position: absolute;
  left: 0px;
  top: 0px;
  visibility: hidden;
}

a.menuItem {
  border: 0px solid #000000;
  color: #003388;
  display: block;
  font-family: MS Sans Serif, Arial, Tahoma,sans-serif;
  font-size: 8pt;
  font-weight: bold;
  padding: 2px 12px 2px 8px;
  text-decoration: none;
}

a.menuItem:hover {
  background-color: #a2a2a2;
  color: #0066FF;
}
span {

  FONT-FAMILY: Verdana, Arial, Helvetica, Tahoma, sans-serif;
  text-decoration: none;
  FONT-WEIGHT: bold;
  cursor: pointer;
}
.my_clip_button {   border: 0px solid #000000;
  color: #003388;
  display: block;
  font-family: MS Sans Serif, Arial, Tahoma,sans-serif;
  font-size: 8pt;
  font-weight: bold;
  padding: 2px 12px 2px 8px;
  text-decoration: none; }
.my_clip_button.hover { background-color: #a2a2a2; color: #0066FF; }
</style>

</HEAD>
<body leftmargin=0 topmargin=0 marginwidth=0 marginheight=0 bgcolor=e2e0e0 onLoad="top.setHP(<?=$user['hp']?>,<?=$user['maxhp']?>)">
<div id="mmoves3" style="background-color:#FFFFCC; visibility:hidden; z-index: 101; overflow:visible; position:absolute; border-color:#666666; border-style:solid; border-width: 1px; padding: 2px;"></div>

<div id=hint3 class=ahint></div>
<div id="mmoves" style="background-color:#FFFFCC; visibility:hidden; z-index: 100; overflow:visible; position:absolute; border-color:#666666; border-style:solid; border-width: 1px; padding: 2px;"></div>
<FORM action="<?=$_SERVER['PHP_SELF']?>" method=POST name="f1" id="f1" onKeyUp="set_action();">
<TABLE width=100% cellspacing=0 cellpadding=0 border=0>
<input type=hidden value='<?=($user['battle']?$user['battle']:$_REQUEST['batl'])?>' name=batl><input type=hidden value='<?=$enemy?>' name=enemy1>
<INPUT TYPE=hidden name=myid value="1053012363">
<TR><TD valign=top>
<TABLE width=250 cellspacing=0 cellpadding=0 id="f1t"><TR>
<TD valign=top width=250 nowrap><CENTER>

<?showpersout($user['id'],1,1,1,1)?>

</TD></TR>
</TABLE>

</td>
<td  valign=top width=80%>
<?

	switch($fbattle->return) {
		case 1 :
if($fbattle->enemy < _BOTSEPARATOR_){
	$unemli = mysql_fetch_array(mysql_query("SELECT login,id,level,invis FROM `users` WHERE `id` = '".$fbattle->enemy."' LIMIT 1;"));
}else{
	$unemli = mysql_fetch_array(mysql_query("SELECT name,id,prototype FROM `bots` WHERE `id` = '".$fbattle->enemy."' LIMIT 1;")); 
	$lvl_bo = mysql_fetch_array(mysql_query("SELECT id,level,invis FROM `users` WHERE `id` = '".$unemli['prototype']."' LIMIT 1;"));
if($lvl_bo){$unemli['level']=$lvl_bo['level']; $unemli['id']=$lvl_bo['id'];}else{$unemli['level']=$user['level'];}
         $unemli['login']=$unemli['name'];

}
			?>
				<TABLE width=100% cellspacing=0 cellpadding=0><TR><TD colspan=2><h3>Поединок</TD></TR>

					<TR><TD><?echo "<b><font color=#003388>".$user['login']." [".$user['level']."]<a href=inf.php?".$user['id']." target=_blank><IMG SRC=i/inf.gif WIDTH=12 HEIGHT=11 ALT=\"Инф. о ".$user['login']."\"></a></b></font>";?></TD>
<? if($unemli['invis']==1) {?>
<TD align=right><?echo "<b><font color=#000>невидимка</b></font>";?></TD>
<?}else{?>
					<TD align=right><?echo "<b><font color=#003388>".$unemli['login']." [".$unemli['level']."]<a href=inf.php?".$unemli['id']." target=_blank><IMG SRC=i/inf.gif WIDTH=12 HEIGHT=11 ALT=\"Инф. о ".$unemli['login']."\"></a></b></font>";?></TD>
<?}?>
				</TR></TABLE>

				<CENTER>
<?

if($user['level'] > 3) {



	if (@$_GET['use']) {
		$dressed=mysql_fetch_row(mysql_query("SELECT id FROM inventory WHERE id=".(int)$_GET['use']." AND dressed='1'"));
		if ((int)$dressed[0]>0) {
			$my_class = $fbattle->my_class;
			ob_start();
			usemagic($_GET['use'],"".$_POST['target']);
			$bb = explode("<!--",ob_get_clean());
			$bb = str_replace('"',"&quot;",(strip_tags($bb[0])));
			Header("Location: ".$_SERVER['PHP_SELF']."?buf=".$bb);
			}
		else die();
	}

	if ($_GET['buf']) {
		echo "<font color=red><b>".$_GET['buf']."</b></font><BR>";
	}



	echoscroll('m1'); echoscroll('m2'); echoscroll('m3'); echoscroll('m4'); echoscroll('m5'); echoscroll('m6'); 
	echoscroll('m7'); echoscroll('m8'); echoscroll('m9'); echoscroll('m10'); echoscroll('m11'); echoscroll('m12');
}
?>

					<TABLE cellspacing=0 cellpadding=0>
					<TR>
						<TD align=center bgcolor=f2f0f0><b>Атака</b></TD>
						<TD>&nbsp;</TD>
						<TD align=center bgcolor=f2f0f0><b>Защита</b></TD>
					</TR>
					<TR><TD>
<?///ново///?>                                                         

<SCRIPT>DrawDots(1);</SCRIPT>
</TD><TD>&nbsp;</TD><TD>
<script>DrawDots(0);</script>


<?/*
					<TABLE cellspacing=0 cellpadding=0>
	   					<TR><TD><INPUT TYPE=radio ID=A1 NAME=attack value=1 onClick="setattack()"><LABEL FOR=A1>удар в голову</LABEL></TD></TR>
						<TR><TD><INPUT TYPE=radio ID=A2 NAME=attack value=2 onClick="setattack()"><LABEL FOR=A2>удар в корпус</LABEL></TD></TR>
    					<TR><TD><INPUT TYPE=radio ID=A3 NAME=attack value=3 onClick="setattack()"><LABEL FOR=A3>удар в пояс(пах)</LABEL></TD></TR>
   					    <TR><TD><INPUT TYPE=radio ID=A4 NAME=attack value=4 onClick="setattack()"><LABEL FOR=A4>удар по ногам</LABEL></TD></TR>

					</TABLE>
				</TD><TD>&nbsp;</TD><TD>

				<TABLE cellspacing=0 cellpadding=0>
					<TR><TD><INPUT TYPE=radio ID=D1 NAME=defend value=1 onClick="setdefend()"><LABEL FOR=D1>блок головы и корпуса</LABEL></TD></TR>
					<TR><TD><INPUT TYPE=radio ID=D2 NAME=defend value=2 onClick="setdefend()"><LABEL FOR=D2>блок корпуса и пояса</LABEL></TD></TR>
					<TR><TD><INPUT TYPE=radio ID=D3 NAME=defend value=3 onClick="setdefend()"><LABEL FOR=D3>блок пояса и ног</LABEL></TD></TR>
					<TR><TD><INPUT TYPE=radio ID=D4 NAME=defend value=4 onClick="setdefend()"><LABEL FOR=D4>блок головы и ног</LABEL></TD></TR>
				</TABLE>
*/?>
				</TD></TR>
				<TR>
					<TD colspan=3 align=center bgcolor=f2f0f0>
<table cellspacing=0 cellpadding=0 width=100%><tr><td><td align=center>
<?
$_SESSION['batl']=$user['battle'];
 ///ново////?> 
<script>DrawButtons();</script>
<?/*
&nbsp;<INPUT TYPE=submit name=go value="Вперед !!!" onClick="this.disabled = true; submit();">
*/?>
</td>

<td align=right><a onClick="location.href='<?=$_SERVER['PHP_SELF']?>?batl=<?=$_REQUEST['batl']?>';"><img src='i/ico_refresh.gif' width=16 height=19 style='cursor:pointer' alt='Обновить'></a></td></tr></table></TD>

				</TR>

				<INPUT TYPE=hidden name=enemy value="<?=$fbattle->enemy?>">
			</TABLE>
<?
$_SESSION['enemy']=$fbattle->enemy;


$res=mysql_query("select slot,id_thing from puton where id_person='".$_SESSION['uid']."' and slot>=201 and slot<=210;");

 while ($s=mysql_fetch_array($res)) {

    $puton[$s['slot']]=$s['slot']; // =new prieminfo($s['id_thing'],0);
    $puton2[$s['slot']]=$s['id_thing'];

  }
$igogo=new ActivePriems($_SESSION['uid']);

?>
<br><script><?
#function DrawRes(SP_HIT, SP_KRT, SP_CNTR, SP_BLK, SP_PRY, SP_HP, SP_SPR, spirit_level){
echo"
DrawRes(".(0+$hit).", ".(0+$krit).", ".(0+$counter).", ".(0+$block).", ".(0+$parry).", ".(0+$hp).", ".(floor($s_duh/100)).", ".str_pad(floor($s_duh/100),strlen($s_duh)+1,'.00',STR_PAD_RIGHT).");
";
#DrawTrick(can_use, img,  txt, free_cast, dsc, resource, select_target, target, target_login, magic_type, name){
#target - friend/enemy/any  друж цель
#free_cast - будет подтверждение
#resource - hit krit counter block parry hp sduh mana zader ispolzovaniy, tratit hod = когда нет freecast
#$myinfo->priems=new ActivePriems($myinfo->id_person);
for ($i=201;$i<=210;$i++) {
	$p=&$puton[$i];
$p2=new prieminfo($puton2[$i],0);

	if ($p) {

		$act=&$igogo->priems[$p2->priem];
		$enable=true;


//////если есть одетые приемы, то втыкаем их в бой!..
if(!$act){
	mysql_query("INSERT INTO `person_on` ( `id_person` , `id_paladin` , `timestamp` , `type` , `timestamp2` ,
						`comment` , `pr_name` , `pr_active` , `pr_wait_for` , `pr_cur_uses` ) VALUES ( '".$_SESSION['uid']."',
						'".$_SESSION['uid']."', NOW( ) , 3, ".time().", NULL , '".$p2->priem."',1,0,1);");

}

		# проверить по статам
		if ($p2->checkbattlehars($myinfo,$hit,$krit,$parry,$counter,$block,$s_duh,$hp)) {
                           $igogo=new ActivePriems($_SESSION['uid']);
                           $act=&$igogo->priems[$p2->priem];
			# можно ипользовать если: прошел срок wait - это в случае если задан wait в параметрах приема
			if ($p2->wait) {

				if ($act['wait']>0) {
					$enable=false;
				}
                                if ($act['active']!=1) {$enable=false;}
			}else{
				if ($act['active']!=1) {$enable=false;}
			}
		}else{

			$enable=false;
		}
		# wait если есть активный - вывод сколько еще
		# uses - определить
		echo "DrawTrick(";
		echo ($enable?'1,':'0,').
		"'".$p2->priem."','"
		.$p2->name."',"
		.($p2->hod?0:1).",'"
		.mysql_escape_string($p2->opisan)."','"
		.(0+$p2->n_hit).",".(0+$p2->n_krit).",".(0+$p2->n_counter).",".(0+$p2->n_block).",".(0+$p2->n_parry).",".(0+$p2->n_hp).",".(0+$p2->sduh).",".(0+$p2->mana).",".(0+$p2->wait).",".($p2->wait?($act['wait']>0?$act['wait']:0):0).",".($p2->maxuses?0+$act['uses']:0).",".(0+$p2->maxuses)."',0,'',0,1,'".$p2->priem."');";
	} else {
		echo"</script><IMG style=\"\" width=40 height=25 src='http://img.combats.com/i/misc/icons/clear.gif'><script>";
	}
}unset($i);
echo"</script>";
if($user['zver_id']>0){
$nb = mysql_fetch_array(mysql_query("SELECT id FROM `bots` WHERE prototype= '".$user['zver_id']."';"));
if($nb){$temn = "style=\"filter:gray(), Alpha(Opacity='70');\""; $ogogo=""; $ogogo2="";}else{$ogogo="<a href=\"?uszver=1\">"; $ogogo2="</a>";}
echo "<br>".$ogogo."<img src=i/sh/pet_unleash.gif ".$temn." onmouseout='hideshow();' onmouseover='fastshow(\"<B>Выпустить зверя</B>\")'>".$ogogo2;
}

?>
			</CENTER>
			<?
		break;
		case 2 :
			if(($user['hp']>0) && $fbattle->battle) {
				echo '<FONT COLOR=red>Ожидаем хода противника...</FONT><BR><CENTER><INPUT TYPE=submit value="Обновить" name=',(($user['battle']>0)?"battle":"end"),'><BR></CENTER>';
			}
			elseif($user['hp'] <= 0 && $fbattle->battle) {
				ref_drop ($user['id']);
				echo '<FONT COLOR=red>Ожидаем, пока бой закончат другие игроки...</FONT><BR><CENTER><INPUT TYPE=submit value="Обновить" name=',(($user['battle']>0)?"battle":"end"),'><BR></CENTER>';
			}
		break;
		case 3 :
			echo "<center><BR>Противник долго не делает свой ход, вы можете закончить бой победителем<BR>
					<INPUT TYPE=submit value=\"Да, я победил!!!\" name=victory_time_out id=\"refreshb\"><BR>";
				if(!$fbattle->user['in_tower'] && $fbattle->user['room']!=200) {
					echo "или признать ничью<BR>
					<INPUT TYPE=submit id=\"refreshb\" value=\"Считаем, что этого боя не было\" name=victory_time_out2><BR>";
				}
			echo "или<BR>
					<INPUT TYPE=submit value=\"Подождать еще немного\" name=",(($user['battle']>0)?"battle":"end"),">
					</center>";
		break;
	}

	if($enemy == 0){
		// проверяем на вшивость
		if(!$fbattle->battle) {
			if($user['battle']) { $ll = $user['battle'];} elseif($_REQUEST['batl']) { $ll = $_REQUEST['batl']; }else{$ll = $_SESSION['batl'];}
			$data = @mysql_fetch_array(mysql_query ("SELECT damage,exp FROM `battle` WHERE `id` = {$ll}"));
			$damage = unserialize($data['damage']);
			$exp = unserialize($data['exp']);
                        if(empty($damage[$user['id']])){$damage[$user['id']]=0;}
			echo '<CENTER><BR>
					<B><FONT COLOR=red>Бой закончен! Всего вами нанесено урона: ',$damage[$user['id']],' HP. Получено опыта: ',(int)$exp[$user['id']],'.</FONT></B>
					<BR><INPUT TYPE=submit value="Вернуться" name="end"><BR></CENTER>';
mysql_query('UPDATE `users` SET `hp2` = 0,`hp3` = 0,`hit` = 0,`s_duh` = 0,`krit` = 0,`counter` = 0,`block2` = 0,`parry` = 0  WHERE `id` = '.$_SESSION['uid'].''); 
mysql_query("DELETE FROM `person_on` WHERE `id_person`='".$_SESSION['uid']."'");
		}
	} else {
?>


</CENTER>

<? }
if($fbattle->battle) {
?>
<HR>
<div id=mes align=center>
<?

//print_r($t1);

	foreach ($fbattle->t1 as $k => $v) {
	if (in_array($v,array_keys($fbattle->battle))) {
		++$i;
		if ($i > 1) { $cc = ', '; } else { $cc = ''; }
		$ffs .= $cc.nick4($v,"B1");
		$zz .= "private [".nick7($v)."] ";
	 }
	}
	$i=0;
?>
<IMG SRC=i/lock.gif WIDTH=20 HEIGHT=15 BORDER=0 ALT="приват" style="cursor:pointer" onClick="Prv('<?=$zz?> ')">
<?=$ffs?>
 <b>против</b>
<?
	$ffs =''; $zz ='';
	foreach ($fbattle->t2 as $k => $v) {
		if (in_array($v,array_keys($fbattle->battle))) {
		++$i;
		if ($i > 1) { $cc = ', '; } else { $cc = ''; }
		$ffs .= $cc.nick4($v,"B2");
		$zz .= "private [".nick7($v)."] ";
		}
	}
	$i=0;
?>
<IMG SRC=i/lock.gif WIDTH=20 HEIGHT=15 BORDER=0 ALT="приват" style="cursor:pointer" onClick="Prv('<?=$zz?> ')">
<?=$ffs?>
<HR>
</div>
<?
} else {
	echo "<HR>";
}
 	if($user['battle']) { $ll = $user['battle'];} elseif($_REQUEST['batl']) { $ll = $_REQUEST['batl']; }else{$ll = $_SESSION['batl'];}
	//$log = mysql_fetch_array(mysql_query("SELECT `log` FROM `logs` WHERE `id` = '".$ll."';"));
	//$log = file("./logs/battle".$ll.".txt");
	$fs = filesize("backup/logs/battle".$ll.".txt");
	$fh = fopen("backup/logs/battle".$ll.".txt", "r");// or die("Can't open file!");
	fseek($fh, -4256, SEEK_END);
	$log[0] = fread($fh, 4256);
	//echo $file;
	fclose($fh);

	$log = explode("<BR>",$log[0]);
	$ic = count($log)-2;

	//echo (int)$fs;

	if ($fs >= 4256) { //($ic-30 >= 0) {
		$max = 1;
		//$max = 1;
	} else {
		$max = 0;
	}
	for($i=$ic;$i>=0+$max;--$i) {


if(eregi("<hr>",$log[$i])){
			$log[$i] = str_replace("<hr>","",$log[$i]); 
                        $log[$i] = $log[$i]."<hr>";
}

		if(eregi(">".$user['login']."</span>",$log[$i])) {
			$log[$i] = str_replace("<span class=date>","<span class=date2>",$log[$i]); 
		}
if(eregi("<hr>",$log[$i])){
		echo $log[$i];
}else{
	echo $log[$i],"<BR>";
}
	}
	unset($ic);
if ($max == 1 ) {
?>
Обрезано для уменьшения объема информации. Полную версию смотрите <a href="logs.php?log=<?=$user['battle']?>" target="_blank">здесь&raquo;</a>
<BR><?}
if(!$user['in_tower']){
?>
<font class=dsc>(Бой идет с таймаутом <?=$fbattle->battle_data['timeout']?> мин.)</font><BR>
<? } ?>
<BR>
На данный момент вами нанесено урона: <B><?=(int)$fbattle->damage[$user['id']]?> HP</B>.

</td>
<TD  valign=top align=rigth>
<TABLE width=250 cellspacing=0 cellpadding=0><TR>
<TD valign=top width=250 nowrap><CENTER>
<?


if($fbattle->return == 1){
	showpersout($fbattle->enemy,1,1,1,0);
}else{

	if ($fbattle->battle_data['type']==4 OR $fbattle->battle_data['type']==5) {
		$a = array(6,16);
		echo "<img src='i/im/",$a[rand(0,1)],".gif'>";
	} elseif ($fbattle->return > 1) {
		echo "<img src='i/im/",rand(1,34),".jpg'>";
	} elseif($exp[$user['id']] > 0) {
		echo "<img src='i/im/",rand(113,115),".jpg'>";
	} else {
		echo "<img src='i/im/",rand(110,112),".jpg'>";
	}
}


?>
</TD></TR>
</TABLE>

</TD></TR>
</TABLE>

</td></tr>
</TABLE>
</FORM>

<!-- <DIV ID=oMenu CLASS=menu onmouseout="closeMenu()"></DIV> -->
<DIV ID="oMenu"  onmouseout="closeMenu()" style="position:absolute; border:1px solid #666; background-color:#CCC; display:none; "></DIV>

</BODY>
</HTML>
<?php
	mysql_query("UNLOCK TABLES;");
//	$fbattle->solve_mf($fbattle->enemy,5);
    //print_r($fbattle->exp);

?>