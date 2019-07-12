
        DROP TABLE IF EXISTS `config_pushes`;
        CREATE TABLE `config_pushes` (
          `conf_id` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
`cn` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
`en` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
      INSERT INTO `config_pushes` (`conf_id`,`cn`,`en`) VALUES ('begin_hit_alliance_boss','~p分钟后开始击杀~pBOSS！','Kill ~p boss after ~p minutes!'),('alliance_boss_killed','~pBOSS已经被击杀！','~p boss was killed!'),('auction_was_surpassed','你竞拍的~p被超越了','你竞拍的~p被超越了');
        DROP TABLE IF EXISTS `config_system_messages`;
        CREATE TABLE `config_system_messages` (
          `conf_id` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
`cn` text COLLATE utf8_unicode_ci,
`en` text COLLATE utf8_unicode_ci
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
      INSERT INTO `config_system_messages` (`conf_id`,`cn`,`en`) VALUES ('kill_alliance_boss_subject','击杀BOSS','击杀BOSS'),('kill_alliance_boss_content','恭喜杀死了~pBoss，你的击杀排名~p','恭喜杀死了~pBoss，你的击杀排名~p'),('joined_alliance_subject','加入联盟','加入联盟'),('joined_alliance_content','已经成功加入了~p联盟','已经成功加入了~p联盟'),('auction_success_subject','竞拍成功','竞拍成功'),('auction_success_content','竞拍~p成功','竞拍~p成功');
        DROP TABLE IF EXISTS `config_item_names`;
        CREATE TABLE `config_item_names` (
          `conf_id` bigint(20) DEFAULT NULL,
`cn` text COLLATE utf8_unicode_ci,
`en` text COLLATE utf8_unicode_ci
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
      INSERT INTO `config_item_names` (`conf_id`,`cn`,`en`) VALUES (90001.0,'道具1','道具1'),(90002.0,'道具2','道具2'),(90003.0,'道具3','道具3'),(90004.0,'道具4','道具4'),(90005.0,'道具5','道具5'),(90006.0,'道具6','道具6'),(90007.0,'道具7','道具7'),(90008.0,'道具8','道具8');