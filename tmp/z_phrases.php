<?
function phrase($sex,$type,$num,$id) { # id �� ������������, �� ���� type=1 ����� ������� �� id �� ���� 
switch ($type) {
case 1: switch($num) {
		case 1: return ($sex?'��������� ���-�� ������':'�������� ���-�� ������');break;
		case 2: return ($sex?'������������':'�����������');break;
		case 3: return ($sex?'����������':'���������');break;
		case 4: return ($sex?'�����������':'����������');break;
		case 5: return ($sex?'������������ �� <�������� ��������>':'����������� �� <�������� ��������>');break;
		case 6: return ($sex?'���������':'��������');break;
		case 7: return ($sex?'���������� � �����':'��������� � �����');break;
		case 8: return ($sex?'����������':'���������');break;
		case 9: return ($sex?'�����������':'����������');break;
		case 10: return ($sex?'����������':'���������');break;
		case 11: return ($sex?'�����������':'����������');break;
		case 12: return ($sex?'������ � ����':'������ � ����');break;
		case 13: return ($sex?'�������� ������������������':'������� ������������������');break;
		case 14: return ($sex?'�������� ����������':'������� ����������');break;
		case 15: return ($sex?'�������� ���-�� �������':'������� ���-�� �������');break;
		case 16: return ($sex?'���������� � ������':'��������� � ������');break;
		case 17: return ($sex?'������������':'�����������');break;
		case 18: return ($sex?'�����������':'����������');break;
		case 19: return ($sex?'���������':'��������');break;
		case 20: return ($sex?'������ <�������� ��������>':'����� <�������� ��������>');break;
		
		case 21: return ($sex?'�������� ������ ��������':'������� ������ ��������');break;
		case 22: return ($sex?'���������� ��������':'����� ����');break;
		case 23: return ($sex?'������ ����':'����� ����');break;
		case 24: return ($sex?'':'');break;
		case 25: return ($sex?'':'');break;
		case 26: return ($sex?'':'');break;
		}
break;
case 2: switch($num) {
		case 1: return ($sex?'':'� �� ���');break;
		case 2: return ($sex?'':'��� �����');break;
		case 3: return ($sex?'':'�');break;
		case 4: return ($sex?'':'�� � ��� �����');break;
		case 5: return ($sex?'':'� ���');break;
		case 6: return ($sex?'':'�� ����� ����������');break;
		case 7: return ($sex?'':'� �����');break;
		case 8: return ($sex?'':'��');break;
		case 9: return ($sex?'':'� ��������');break;
		case 10: return ($sex?'':'�� ����������');break;

		case 11: return ($sex?'':'� � ���� ������');break;
		case 12: return ($sex?'':'� � �� �� �������');break;
		case 13: return ($sex?'':'� � ���� ���');break;
		case 14: return ($sex?'':'');break;
		case 15: return ($sex?'':'');break;
		case 16: return ($sex?'':'');break;
		}
break;
case 3: switch($num) {
		case 1: return ($sex?'�����������':'�����������');break;
		case 2: return ($sex?'������������':'������������');break;
		case 3: return ($sex?'������������':'������������');break;
		case 4: return ($sex?'�����������':'�����������');break;
		case 5: return ($sex?'������������':'������������');break;
		case 6: return ($sex?'��������':'��������');break;
		case 7: return ($sex?'����������':'����������');break;
		case 8: return ($sex?'������������':'��������');break;
		case 9: return ($sex?'������������':'������������');break;
		case 10: return ($sex?'�����������':'�����������');break;
		case 11: return ($sex?'������':'������');break;
		case 12: return ($sex?'��������':'��������');break;
		case 13: return ($sex?'��������������':'��������������');break;
		case 14: return ($sex?'��������':'��������');break;
		case 15: return ($sex?'�������':'�������');break;
		case 16: return ($sex?'������������':'������������');break;
		case 17: return ($sex?'�����������':'�����������');break;
		case 18: return ($sex?'������':'������');break;
		case 19: return ($sex?'������':'������');break;
		case 20: return ($sex?'�������':'�������');break;

		case 21: return ($sex?'��������':'��������');break;
		case 22: return ($sex?'��������':'��������');break;
		case 23: return ($sex?'�������':'�������');break;
		case 24: return ($sex?'����������':'����������');break;
		case 25: return ($sex?'�������':'�������');break;
		case 26: return ($sex?'�������':'�������');break;
		case 27: return ($sex?'�����������':'�����������');break;
		case 28: return ($sex?'':'');break;
		case 29: return ($sex?'':'');break;
		}
break;
case 4: switch($num) {
		case 1: return ($sex?'':'��������');break;
		case 2: return ($sex?'':'��������');break;
		case 3: return ($sex?'':'�����������');break;
		case 4: return ($sex?'':'���������');break;
		case 5: return ($sex?'':'������������');break;
		case 6: return ($sex?'':'�����');break;
		case 7: return ($sex?'':'�� �����');break;
		case 8: return ($sex?'':'�� ����� ���� �� ������');break;
		case 9: return ($sex?'':'��������');break;
		case 10: return ($sex?'':'�������������');break;
		case 11: return ($sex?'':'������������');break;
		case 12: return ($sex?'':'�����������');break;
		case 13: return ($sex?'':'�����������');break;
		case 14: return ($sex?'':'�����������');break;
		case 15: return ($sex?'':'������������');break;
		case 16: return ($sex?'':'������');break;
		case 17: return ($sex?'':'������ ������� ������');break;
		case 18: return ($sex?'':'�� �������');break;
		case 19: return ($sex?'':'�������');break;
		case 20: return ($sex?'':'�������');break;
		case 21: return ($sex?'':'� ������');break;
		}
break;
case 5: switch($num) {
		case 1: return ($sex?'��������':'�������');break;
		case 2: return ($sex?'�������':'������');break;
		case 3: return ($sex?'�������':'������');break;
		}
break;
case 6: switch($num) {
		case 1: return ($sex?'���������':'��������');break;
		case 2: return ($sex?'�������':'������');break;
		}
break;
case 7: switch($num) {
		case 1: return ($sex?'���������':'��������');break;
		case 2: return ($sex?'��������':'�������');break;
		}
break;
case 8: switch($num) {
		case 1: return ($sex?'����������':'���������');break;
		}
break;
case 9: switch($num) {
		case 1: return ($sex?'��������':'������');break;
		}
break;
case 10: switch($num) {
		case 1: return ($sex?'':'������');break;
		case 2: return ($sex?'':'������');break;
		case 3: return ($sex?'':'��������');break;
		}
break;
case 11: switch($num) {
		case 1: return ($sex?'':'�����');break;
		}
break;
case 12: switch($num) {
		case 1: return ($sex?'':'������� ����');break;
		}
break;
case 13: switch($num) {
		case 1: return ($sex?'':'����������� ����');break;
		}
break;
case 14: switch($num) {
		case 1: return ($sex?'':'������� ����');break;
		case 2: return ($sex?'':'����������� ����');break;
		}
break;
case 15: switch($num) {
		case 1: return ($sex?'':'������������� ����');break;
		case 2: return ($sex?'':'�������� ����');break;
		}
break;
case 16: switch($num) {
		case 1: return ($sex?'':'���������� ����');break;
		}
break;
case 17: switch($num) {
		case 1: return ($sex?'':'������� ������');break;
		case 2: return ($sex?'':'�������� �������');break;
		case 3: return ($sex?'':'�������� ������');break;
		case 4: return ($sex?'':'�������������� �������');break;
		case 5: return ($sex?'':'������� �������');break;
		case 6: return ($sex?'':'�������� �������');break;
		case 7: return ($sex?'':'������� �����');break;
		case 8: return ($sex?'':'��������� �����');break;
		}
break;
case 18: switch($num) {
		case 1: return ($sex?'':'�������');break;
		}
break;
case 19: switch($num) {
		case 1: return ($sex?'':'������ �����');break;
		}
break;
case 20: switch($num) {
		case 1: return ($sex?'':'�������');break;
		}
break;
case 21: switch($num) {
		case 1: return ($sex?'':'������� ����');break;
		case 2: return ($sex?'':'������ �����');break;
		case 3: return ($sex?'':'������� ����');break;
		case 4: return ($sex?'':'������� �������� ������ ����');break;
		case 5: return ($sex?'':'��������� ����');break;
		case 6: return ($sex?'':'������');break;
		case 7: return ($sex?'':'�������');break;
		}
break;
case 22: switch($num) {
		case 1: return ($sex?'':'������� �������');break;
		}
break;
case 23: switch($num) {
		case 1: return ($sex?'':'��������� ���������');break;
		case 2: return ($sex?'':'������� �� �������� ������');break;
		case 3: return ($sex?'':'������ ������');break;
		}
break;
case 24: switch($num) {
		case 1: return ($sex?'':'��������');break;
		}
break;
case 25: switch($num) {
		case 1: return ($sex?'':'� ������ ����');break;
		case 2: return ($sex?'':'� ���');break;
		}
break;
case 26: switch($num) {
		case 1: return ($sex?'':'��� �������');break;
		case 2: return ($sex?'':'� �����');break;
		case 3: return ($sex?'':'� ������ ������');break;
		case 4: return ($sex?'':'� �������');break;
		case 5: return ($sex?'':'� ����');break;
		}
break;
case 27: switch($num) {
		case 1: return ($sex?'':'� ������ �����');break;
		case 2: return ($sex?'':'� ����� �����');break;
		case 3: return ($sex?'':'� ����� ���');break;
		case 4: return ($sex?'':'� �� ������');break;
		case 5: return ($sex?'':'� ���������');break;
		case 6: return ($sex?'':'� �����');break;
		case 7: return ($sex?'':'� ����� ������');break;
		}
break;
case 28: switch($num) {
		case 1: return ($sex?'':'� ���');break;
		case 2: return ($sex?'':'�� ������');break;
		case 3: return ($sex?'':'� �������������� ������� ����');break;
		case 4: return ($sex?'':'� ������');break;
		}
break;
case 29: switch($num) {
		case 1: return ($sex?'':'� ���������� �����');break;
		case 2: return ($sex?'':'�� �����');break;
		case 3: return ($sex?'':'� ������');break;
		case 4: return ($sex?'':'� ������');break;
		case 5: return ($sex?'':'� ������ ����');break;
		case 6: return ($sex?'':'� ����� ����');break;
		}
break;
case 30: switch($num) {
		case 1: return ($sex?'':'���������');break;
		case 2: return ($sex?'':'�����');break;
		case 3: return ($sex?'':'����������');break;
		case 4: return ($sex?'':'���������');break;
		}
break;
case 31: switch($num) {
		case 1: return ($sex?'������':'�����');break;
		case 2: return ($sex?'�����':'����');break;
		case 3: return ($sex?'���������':'��������');break;
		case 4: return ($sex?'��������� ���':'�������� ���');break;
		}
break;
case 32: switch($num) {
		case 1: return ($sex?'':'������ ���');break;
		case 2: return ($sex?'':'������� ����� ���� �����');break;
		}
break;
case 33: switch($num) {
		case 1: return ($sex?'����� � ����� � ������ ��������� ��������':'���� � ����� � ����� ��������� ��������');break;
		case 2: return ($sex?'�������� �� ���������, � �������� ��������':'������� �� ���������, � ������� ��������');break;
		case 3: return ($sex?'��������� ������ ���� ��������� ���, �������� ��������':'��������� ������ ���� ��������� ���, ������� ��������');break;
		case 4: return ($sex?'�����������, ��� ������ ����� �������� ����, ���������� ��������':'�����������, ��� ������ ����� �������� ����, �������� ��������');break;
		}
break;
case 34: switch($num) {
		case 1: return ($sex?'�������':'�����');break;
		case 2: return ($sex?'���������':'��������');break;
		}
break;
case 35: switch($num) {
		case 1: return ($sex?'':'����');break;
		}
break;
case 36: switch($num) {
		case 1: return ($sex?'����������':'���������');break;
		case 2: return ($sex?'��������� ���� ��':'�������� ���� ��');break;
		case 3: return ($sex?'�����������':'����������');break;
		case 4: return ($sex?'������':'�����');break;
		case 5: return ($sex?'����������':'���������');break;
		}
break;
case 37: switch($num) {
		case 1: return ($sex?'������ �����':'����� �����');break;
		}
break;
case 38: switch($num) {
		case 1: return ($sex?'�� �������������� ��������':'�� ������������� ��������');break;
		case 2: return ($sex?'����������':'���������');break;
		case 3: return ($sex?'��������� �������� ����':'�������� �������� ����');break;
		case 4: return ($sex?'������ � <�������� ��������>':'����� � <�������� ��������>');break;
		case 5: return ($sex?'����������':'���������');break;
		case 6: return ($sex?'�������� ������������':'������� ������������');break;
		case 7: return ($sex?'�������� ������':'������� ������');break;
		case 8: return ($sex?'���� ������� �����������':'��� ������� ����������');break;
		case 9: return ($sex?'�������� �������� ����':'������� �������� ����');break;
		case 10: return ($sex?'��������������':'�������������');break;
		case 11: return ($sex?'�� ������ � ���':'�� ����� � ���');break;
		case 12: return ($sex?'�� ���������� ���� ����':'�� ��������� ���� ����');break;
		case 13: return ($sex?'������������':'�����������');break;
		case 14: return ($sex?'����������':'���������');break;
		case 15: return ($sex?'������ �� � ���':'����� �� � ���');break;
		}
break;
case 39: switch($num) {
		case 1: return ($sex?'':', � ������');break;
		case 2: return ($sex?'':' �');break;
		case 3: return ($sex?'':', ��');break;
		case 4: return ($sex?'':', ���������� ����');break;
		case 5: return ($sex?'':', ������');break;
		}
break;
case 40: switch($num) {
		case 1: return ($sex?'���� ������ �� �����':'���� ������ �� �����');break;
		case 2: return ($sex?'���� ����� �� �����':'���� ����� �� �����');break;
		case 3: return ($sex?'����� ����':'���� ����');break;
		case 4: return ($sex?'���������� �� �����':'��������� �� �����');break;
		case 5: return ($sex?'���������� �� �����':'��������� �� �����');break;
		}
break;
case 41: switch($num) {
		case 1: return ($sex?'':'������ ������ 100 �����');break;
		case 2: return ($sex?'':'������ "��!"');break;
		case 3: return ($sex?'':'������� ���������� �����');break;
		case 4: return ($sex?'':'� ��������� �����');break;
		case 5: return ($sex?'':'������� ����� ��� ������');break;
		}
break;
case 42: switch($num) {
		case 1: return ($sex?'':'�<�������� ��������>���');break;
		case 2: return ($sex?'':'���������');break;
		}
break;
case 43: switch($num) {
		case 1: return ($sex?'������� �� ����� ����������':'������ �� ����� ����������');break;
		case 2: return ($sex?'������� � ��c':'������ � ��c');break;
		case 3: return ($sex?'����������� ���':'���������� ���');break;
		case 4: return ($sex?'�������� �� ������ �����':'������� �� ������ �����');break;
		}
break;
case 44: switch($num) {
		case 1: return ($sex?'':'{datetime} � � ���� ������');break;
		case 2: return ($sex?'':'���� ���������� {datetime}, �����');break;
		}
break;
case 45: switch($num) {
		case 1: return ($sex?'��� ������� �� ������ �����':'��� ������� ��� ������ �����');break;
		case 2: return ($sex?'��� �� �������� ��� �����':'��� ��� �������� ��� �����');break;
		#case 2: return ($sex?'��� �� �������� ��� �����':', �������� ������ ���������, �����, ��� ��� �������� ��� �����');break;
		}
break;
case 46: switch($num) {
		case 1: return ($sex?'������, ��������� ��������� ���� � ������':'�����, ��������� ��������� ���� � ������');break;
		case 2: return ($sex?', �������� ������ ���������, ������':', �������� ������ ���������, �����');break;
		}
break;
case 47: switch($num) {
		case 1: return ($sex?'�������� �����, ������� � �������� � �����':'�������� �����, ������� � �������� � �����');break;
		#case 2: return ($sex?', �������� ������ ���������, ������':', �������� ������ ���������, �����');break;
		}
break;
case 48: switch($num) {
		case 1: return ($sex?', �������� ����� ������ ������, �� ��������� ��� ��������� ����� ':', �������� ����� ������ ������, �� ��������� ��� �������� ����� ');break;
		}
break;
case 49: switch($num) {
		case 1: return '���� �� ������ �������� ������';break;
		case 2: return '���� �� ������ ����� ������ � ����� � ������';break;
		case 3: return '���-��� ��������� ���-�� ���������� �� ���� ���';break;
		}
break;
case 50: switch($num) {
		case 1: return ($sex?'���������� �� ���� �������':'��������� �� ���� �������');break;
		case 2: return ($sex?'��������� �����':'�������� �����');break;
		case 3: return ($sex?'���������� ����� �����':'��������� ����� �����');break;
		}
break;

#12:04 �������� �����, ������� � �������� � ����� "���������� ������" ������� ��������������� ������������ ��� �������.
}
if (!$type) {
	$res=db_use('array',"select * from phrases where id_phrase='".$id."'");
	return ($sex?$res['phrasew']:$res['phrasem']);
}

}
function phrases($type) # ���������� ���� ����
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
function phrase_protknul($type_att) { # ��������/������
	if ($type_att==2) { $a=6;} #�����
	elseif ($type_att==3) { $a=7;} #�����
	elseif ($type_att==4) { $a=8;} #������
	elseif ($type_att==5) { $a=9;} #�����
	else { $a=5;} #�������
	return $a;
}
function phrase_type_att($type_att) { # ����������� ����
	if ($type_att==2) { $a=11;} #�����
	elseif ($type_att==3) { $a=12;} #�����
	elseif ($type_att==4) { $a=13;} #������
	elseif ($type_att==5) { $a=14;} #�����
	elseif ($type_att==6) { $a=15;} #�����
	elseif ($type_att==7) { $a=16;} #����
	else{ $a=35;} #�������
	return $a;
}
function phrase_kuda($kuda){ # � ������ / � ������
	if ($kuda==1) { $a=25;} #������
	elseif ($kuda==2) { $a=26;} #�����
	elseif ($kuda==3) { $a=27;} #�����
	elseif ($kuda==4) { $a=28;} #����
	elseif ($kuda==5) { $a=29;} #����
	return $a;
}
?>