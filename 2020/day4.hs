import Data.List

input = "eyr:2024 pid:662406624 hcl:#cfa07d byr:1947 iyr:2015 ecl:amb hgt:150cm\n\niyr:2013 byr:1997 hgt:182cm hcl:#ceb3a1\neyr:2027\necl:gry cid:102 pid:018128535\n\nhgt:61in iyr:2014 pid:916315544 hcl:#733820 ecl:oth\n\nhcl:#a97842\neyr:2026 byr:1980 ecl:grn pid:726519569 hgt:184cm cid:132 iyr:2011\n\necl:grn hcl:#6b5442 pid:619743219 cid:69 hgt:176cm eyr:2027 iyr:2012\nbyr:1980\n\necl:brn byr:1969 iyr:2014\nhgt:164cm eyr:2020 pid:982796633 hcl:#602927\n\necl:gmt\niyr:1987 eyr:2039 pid:15115163 byr:2006\nhcl:bfab0d\n\ncid:117\nhcl:#efcc98\niyr:2010 pid:322719183\nhgt:176cm\neyr:2020\nbyr:1957\necl:brn\n\nbyr:1954 hgt:178cm hcl:#38f7fd pid:838813262 ecl:blu\neyr:2029 iyr:2019\n\neyr:2023 ecl:amb iyr:2020 byr:1927 pid:242570886 hcl:#18171d hgt:192cm\n\niyr:1990 cid:295 hgt:131 pid:187cm byr:2014\necl:xry hcl:z\neyr:1928\n\necl:hzl\nbyr:1953\neyr:2023 hcl:#866857\nhgt:181cm iyr:2010 pid:568185567\n\nbyr:2030 hcl:#fffffd ecl:#a4a596 hgt:168cm\niyr:1936 eyr:2020 cid:296 pid:168786676\n\nbyr:2030 iyr:2026 eyr:1974 hcl:7fcaa5 ecl:utc\npid:190cm\nhgt:67cm\n\nbyr:2023 eyr:2037 hgt:59cm\necl:lzr hcl:z iyr:2026 pid:#ea9083\n\nbyr:2003 hcl:z hgt:91 iyr:1990 eyr:2024 ecl:#123d73\npid:48494230\n\nbyr:2022 eyr:2020 iyr:2030 ecl:gmt\nhgt:191cm pid:3509331253 hcl:#888785\n\niyr:1994\necl:#c3d564 byr:2009\nhgt:162cm hcl:336498 pid:#e99d09\ncid:288\neyr:1921\n\nbyr:1924 cid:290 iyr:2010 ecl:amb eyr:2020\nhgt:156cm hcl:#7d3b0c pid:795497164\n\ncid:301 iyr:2017 hgt:67cm\nhcl:#888785 ecl:#0405b9 byr:1964 pid:707857518 eyr:1976\n\necl:gry pid:474303066\niyr:2011 hcl:#18171d hgt:165cm byr:1921 eyr:2024\n\nhcl:#6b5442 ecl:amb iyr:2020 hgt:191cm\nbyr:1949 cid:301\npid:075846582 eyr:2029\n\nhcl:#a97842 cid:186 iyr:2014\necl:gry\nhgt:191cm eyr:2023 pid:645548969\nbyr:1956\n\npid:154cm hcl:z ecl:gmt iyr:1989 hgt:69in cid:53 byr:2010\n\nhgt:72cm byr:2023\neyr:2034 hcl:z ecl:#f5249e iyr:1997 pid:#79af7a\n\neyr:2038 byr:2015\nhgt:70cm ecl:grt hcl:9d58a1 iyr:1926 pid:6290928420\n\npid:620857794 eyr:2022\nbyr:1950\nhgt:159cm\nhcl:#ceb3a1 ecl:amb iyr:2015\n\neyr:1954 ecl:#ab2ce4 pid:#14eedd\niyr:2009\nhcl:29e484\nbyr:2022 hgt:73cm\n\nhgt:59cm byr:2026 cid:245 iyr:2020\neyr:2029 pid:073943129 ecl:hzl\nhcl:#b6652a\n\niyr:2014 byr:2015 hcl:#a97842 eyr:2029\npid:#132098\nhgt:150 ecl:oth\n\nhgt:151in ecl:#967d49 eyr:2026 hcl:#18171d\npid:384230726 byr:1934\niyr:2018\n\niyr:2020 eyr:2021 byr:1937 pid:735047371 cid:159 ecl:blu hgt:177cm hcl:#22b774\n\necl:brn hcl:#6b5442 pid:117807698 cid:105 iyr:2016 byr:1977 hgt:183cm\n\necl:hzl hcl:#6b5442 byr:1933\niyr:2019 pid:348486702\neyr:2020 hgt:193cm\n\nbyr:1928\necl:gry\neyr:2028 hcl:#fffffd pid:571149069\niyr:2012 hgt:175cm\n\npid:359108298\neyr:2027 hgt:158cm ecl:amb iyr:2016\nhcl:#602927\n\niyr:2027 byr:2015\nhgt:191in pid:102033301 ecl:xry\neyr:2031 hcl:#602927\n\necl:oth cid:163 hcl:z iyr:2014\nbyr:1944 hgt:173cm\neyr:2027 pid:#0524c1\n\necl:brn\nbyr:2030 hgt:71cm eyr:1931 cid:165 iyr:2010 hcl:#cfa07d\npid:509642098\n\nhgt:166 iyr:2020 cid:308\neyr:2022 pid:950463527\nbyr:2017\nhcl:z\n\necl:amb\neyr:2023 byr:1924\npid:901038027 hgt:70in\niyr:2010 hcl:z\n\nbyr:1972\niyr:2013\nhcl:d669ad hgt:64cm cid:247 ecl:#19aa26 eyr:2023\n\nhgt:71 hcl:#fffffd\nbyr:1976 cid:108 eyr:2038\necl:grt iyr:2018 pid:190cm\n\niyr:2017\nbyr:1963 ecl:grn hgt:175cm\npid:160915270 eyr:2028 hcl:#cfa07d\n\npid:569740130 hgt:171cm hcl:#733820\necl:gry eyr:2024 iyr:2020 byr:1973\n\nbyr:1937\niyr:2016 ecl:gry hgt:181cm pid:521705827 hcl:#b6652a eyr:2027 cid:295\n\nhgt:156cm ecl:blu iyr:2019 hcl:#866857\npid:662418718 byr:2000 eyr:2024\n\nbyr:1971 pid:693616099\nhcl:#efcc98\nhgt:175cm iyr:2016 ecl:gry\neyr:2023\n\niyr:2013\neyr:2024\necl:gry\npid:414295491 byr:1986\nhgt:188cm hcl:#b6652a\n\neyr:2022 byr:1975 iyr:2020\necl:grn cid:68 hcl:#a97842\nhgt:151cm pid:229803943\n\ncid:258 iyr:2012\necl:hzl\nbyr:2001\neyr:2021\nhcl:#866857 pid:990590217 hgt:172cm\n\ncid:339 byr:1957 hcl:#866857 pid:343480061 eyr:2039\nhgt:191cm\niyr:2021\necl:utc\n\ncid:281 hcl:z ecl:blu\nbyr:2020 pid:132694306 eyr:2020 iyr:1953\n\nhcl:#602927\nbyr:1933 eyr:2028\nhgt:165cm ecl:gry iyr:2018 pid:658484617\n\necl:oth\nhgt:188cm cid:110 pid:056975690 iyr:2016 byr:1950 eyr:2023 hcl:#cfa07d\n\ncid:342 hcl:#fffffd eyr:2024\npid:153555359 byr:1974\necl:gry hgt:191cm iyr:2020\n\nbyr:2019 ecl:#160ed3 eyr:1999 hcl:z\ncid:146 pid:195693972 hgt:159cm\n\niyr:2015 eyr:2030 hgt:191cm byr:1979\necl:#ec4873 pid:994113786 hcl:#cfa07d\n\npid:552331609\necl:grn\nhgt:171cm eyr:2022 hcl:#b6652a\niyr:2020 byr:1931\n\nhgt:177cm iyr:2010 pid:934058099\neyr:2020\necl:blu\nbyr:1967\ncid:112 hcl:#7d3b0c\n\niyr:2028\nhgt:138\ncid:180 hcl:z\neyr:2022 pid:3286566621 byr:2002\n\neyr:2020\niyr:2019\nhcl:#a97842 pid:149148750 ecl:brn hgt:159cm\nbyr:1981 cid:339\n\ncid:344\neyr:2021 byr:1968 pid:777786047\necl:grn hgt:192cm hcl:#888785\niyr:2015\n\nhgt:173cm\neyr:2030\nhcl:#733820 pid:610226642 byr:1954 cid:80\niyr:2013 ecl:blu\n\nbyr:1999 eyr:2023\necl:amb pid:912145128\nhgt:181cm\niyr:2015 hcl:#a97842\n\neyr:2027 hgt:188cm\npid:080715145 hcl:#341e13 iyr:2013\necl:oth\nbyr:1965\n\nhgt:170cm byr:1950 iyr:2013\npid:010541784\neyr:2027 ecl:zzz\nhcl:a3bae8\n\nhgt:190cm eyr:2024 ecl:#6dcedc pid:909319684\niyr:2011 byr:1959 hcl:z cid:182\n\neyr:2028\niyr:2016 hcl:#623a2f pid:208417572 byr:1929 cid:137 ecl:hzl\nhgt:167cm\n\nhcl:#6b5442\necl:grn\nbyr:1938\neyr:2023 cid:307\nhgt:59in iyr:2014 pid:205268145\n\npid:047489285 eyr:2026\nhcl:#b6652a byr:1920\niyr:2015\nhgt:183cm ecl:gry\n\necl:blu hcl:#508e8b iyr:2016 eyr:1954 hgt:151cm pid:086752750 byr:1920\n\niyr:2011 byr:1981 hgt:186cm\ncid:117 hcl:#6b5442 ecl:amb\npid:756830713 eyr:2026\n\neyr:2037 pid:364464758 hcl:z ecl:grn\nhgt:112 iyr:2013 byr:2022\n\necl:hzl\ncid:65 pid:679487194\nbyr:1986 hgt:169cm hcl:#cfa07d eyr:2025 iyr:2013\n\ncid:192\nbyr:1921 pid:#5fe831 ecl:#fbb2b9 hgt:62cm eyr:1971 iyr:2024\nhcl:z\n\nhcl:#cfa07d eyr:2026\nhgt:74in\niyr:2019\necl:xry\npid:622690982 byr:1982\n\neyr:2026 pid:523515724 iyr:2013 byr:1973 hgt:167cm\necl:grn hcl:#866857\n\nbyr:2009\neyr:1985 pid:484497014 ecl:#0bfcf2 iyr:1992 cid:131 hcl:39d6b0 hgt:177in\n\neyr:2020 iyr:2016 ecl:brn hcl:#ceb3a1 byr:1966 pid:696621560 cid:62\nhgt:59in\n\nhgt:166cm hcl:#7d3b0c\niyr:2016\necl:brn pid:190cm\neyr:2020\nbyr:2001\n\neyr:2021\niyr:2012 hcl:#6b5442\necl:amb hgt:169cm\npid:969150085\nbyr:1925\n\necl:brn hgt:175cm byr:1992 iyr:2016 pid:415209726 eyr:2027\ncid:72 hcl:#866857\n\niyr:2017\nhcl:#733820 byr:1938 eyr:2020 pid:274486958 hgt:163cm\n\nhcl:4f5dd1 cid:336 ecl:grn iyr:1931 pid:6212280197\nbyr:2016 eyr:2037\nhgt:187in\n\niyr:2017 byr:1940 eyr:2025 pid:115098205 hgt:151cm\necl:grn\ncid:122\nhcl:#6b5442\n\nhcl:#efcc98\niyr:2020 pid:709548547 hgt:179cm\neyr:2030 ecl:gry byr:1975\n\ncid:217 hcl:#888785 eyr:2029\necl:hzl iyr:2013 pid:160053490\nhgt:166cm byr:1992\n\neyr:2024 cid:188 iyr:2016 hcl:ff3a59 ecl:xry pid:296357512 byr:2026\n\nhgt:154cm iyr:2010\necl:blu pid:717041634 byr:1928 cid:123\neyr:2027\nhcl:#a97842\n\npid:391011205 ecl:hzl hgt:191cm iyr:2016 eyr:2028 cid:281 byr:1934\n\nbyr:1937 hgt:65in\npid:667975382 ecl:gry cid:270 eyr:2024\niyr:2012\n\nhgt:179cm pid:065528723\nhcl:#888785 byr:1937 eyr:2028\niyr:2013 ecl:hzl\n\niyr:2027 cid:261 eyr:2037 ecl:#ced7d5 pid:157cm\nhcl:3a80c1 byr:2029 hgt:187in\n\neyr:2028\nhgt:157cm hcl:#733820\niyr:2012 ecl:blu byr:1952 pid:915063263 cid:335\n\neyr:2023 hcl:#efcc98 pid:490625944 byr:1961 ecl:grn hgt:155cm iyr:2018\n\ncid:247 pid:2807544665 eyr:2021\necl:oth\nhgt:191cm\nbyr:1928\niyr:2013 hcl:#623a2f\n\neyr:2015\nbyr:2021\nhcl:40d2fc hgt:69cm pid:159cm ecl:gmt\n\nhgt:175cm eyr:1992 cid:328 pid:263110997 ecl:#e53989 byr:2014 hcl:#a97842 iyr:2026\n\npid:491396731 eyr:2027 hgt:172cm hcl:#623a2f cid:92 iyr:2017 byr:1983 ecl:grn\n\nhcl:#fffffd\niyr:2018 byr:1983 pid:714591144 ecl:grn eyr:2021\nhgt:160cm\n\neyr:2027\nhgt:63in ecl:blu byr:1987 pid:397963077 iyr:2018 hcl:#ceb3a1\n\neyr:2027\nhgt:184cm\nhcl:#6b5442 iyr:2012 byr:1984 ecl:blu pid:196287205\n\niyr:1998\necl:hzl\npid:7872103596 byr:1991\ncid:275 eyr:2039\nhgt:174cm hcl:0d2ad6\n\niyr:2010 hcl:#efcc98\nbyr:1992 hgt:65cm eyr:2038 pid:383236012 cid:68 ecl:lzr\n\nhgt:190in cid:127\nbyr:1947 pid:515728209 hcl:#733820 iyr:2014 ecl:amb eyr:2020\n\niyr:2017 eyr:2028\nhcl:#623a2f\nbyr:1964 ecl:grn pid:198467794 hgt:169cm\n\necl:utc\nhgt:59cm byr:2007 iyr:2030\nhcl:7ac4db eyr:2038 pid:#7206c6\n\niyr:2010\nhcl:z eyr:2021 ecl:brn\nhgt:173 cid:86\npid:194240791 byr:1975\n\npid:9347286034\nhgt:63cm\niyr:1992 eyr:2034 hcl:66031b ecl:grt byr:1929\n\npid:593398904 byr:1939 iyr:2019 hcl:#b6652a ecl:gry eyr:2023\nhgt:70cm\n\nbyr:1991\niyr:2019 hgt:164cm pid:282852411 cid:340 ecl:amb\nhcl:#341e13 eyr:2027\n\neyr:2020\niyr:2014 ecl:grn hcl:#866857 hgt:158cm\nbyr:1931 pid:321748597\n\ncid:98 byr:2023 iyr:2019 pid:#48f79f\nhcl:73c882 eyr:1973 hgt:151in\necl:utc\n\niyr:2023\nhcl:#18171d\npid:52221892 eyr:2039\nbyr:2008 hgt:72cm ecl:#db8d14\n\niyr:1966 cid:274\neyr:2034 pid:12256322\nbyr:2006 ecl:dne\nhcl:985c2d\n\nhcl:#fd033b\neyr:2026 ecl:blu\niyr:2016\nbyr:1953 hgt:157cm\npid:502619036\n\nbyr:2015 pid:159cm iyr:2025\nhgt:158cm eyr:1943 hcl:z ecl:grn\n\necl:blu iyr:2016\npid:842400950\nhcl:#733820\ncid:266\neyr:2027 byr:1931\nhgt:161cm\n\niyr:2017 hgt:190cm byr:1994 pid:706570967\necl:hzl hcl:#18171d\ncid:180\n\ncid:197 pid:204952666 ecl:amb\nhgt:70in iyr:2016 byr:1936 hcl:#98cbe3 eyr:2025\n\npid:555499128\nbyr:1971 hgt:71in\ncid:83 ecl:blu\nhcl:#cfa07d eyr:2027\n\necl:hzl iyr:2014\npid:30428184 cid:237\nhgt:171cm byr:1942 hcl:#888785 eyr:1986\n\neyr:2025\npid:579385370 hgt:193cm\nhcl:#c0946f byr:1979 iyr:2016\necl:amb cid:284\n\neyr:2029 byr:1946 pid:278271295\necl:grn\nhcl:#cfa07d cid:271\nhgt:172cm\niyr:2020\n\npid:731752614 eyr:2020 byr:1983\ncid:248 ecl:oth hgt:179cm\niyr:2017 hcl:#fffffd\n\nhcl:z\ncid:203 eyr:2032 ecl:#3f9d3d hgt:65cm pid:4042846885 byr:2019\niyr:1946\n\nhgt:171cm ecl:gry eyr:2027\niyr:2013\nhcl:#7d3b0c pid:92288579\nbyr:1955\n\necl:brn hgt:164cm byr:1969 hcl:#cbf9c9 pid:022724981 eyr:2030 iyr:2013 cid:244\n\nhgt:162cm byr:1974 iyr:2015 pid:927525094 hcl:#3d3011 ecl:blu\neyr:2023\n\nhgt:157cm\neyr:2020\npid:221286943 hcl:#fffffd ecl:amb iyr:2018 byr:1945\n\niyr:2019\neyr:2025 byr:1997 pid:341544323 hgt:174cm cid:113\necl:hzl\n\npid:138492032 hcl:e35302 ecl:#caaede\neyr:1931\nbyr:2001 hgt:156 iyr:1998\n\npid:912182030 cid:189 hgt:162 hcl:#277b39\niyr:2013 eyr:2023 byr:2023 ecl:blu\n\neyr:2027 hcl:#fffffd\necl:brn\ncid:304 iyr:2016 byr:1969\npid:866607511 hgt:192cm\n\nhgt:64in\necl:amb\nbyr:1958\npid:720439412\niyr:2015 eyr:2022 hcl:#ceb3a1\n\neyr:2024 hgt:159cm\npid:187867283 iyr:2016\necl:oth hcl:#fffffd\nbyr:1988\n\necl:#910bf2 byr:1969 iyr:2011 hcl:z eyr:2024 pid:579502502\ncid:103 hgt:174cm\n\npid:718692455\neyr:2028\niyr:2016\nhcl:#602927\necl:blu byr:1954\ncid:251 hgt:182cm\n\neyr:2021 hcl:#341e13 ecl:amb\nbyr:1933 hgt:179cm iyr:2011 pid:083172316\n\niyr:1998 hcl:z eyr:1944\nbyr:2006 pid:453368738\nhgt:160 ecl:#9da5f1 cid:261\n\nhcl:#7d3b0c\niyr:2018\nhgt:164cm eyr:2020 byr:1940 ecl:blu\n\npid:993701676 eyr:2028 ecl:gry\nbyr:1951 hcl:#888785 cid:116\niyr:2020\nhgt:192cm\n\nhcl:z eyr:2033\necl:lzr iyr:2029 cid:326 hgt:68cm byr:2026\npid:96742419\n\nhcl:#a97842 ecl:brn\nbyr:1920\nhgt:173cm iyr:2015\neyr:2024 pid:176967666\n\nbyr:1930 eyr:2025 pid:792694131\nhgt:179cm ecl:brn\nhcl:#a97842\niyr:2015\n\nhgt:167cm byr:1960 eyr:2022 hcl:#efcc98\ncid:87 ecl:blu iyr:2012\npid:431515059\n\nhcl:#cfa07d\neyr:2023\nhgt:188cm ecl:grn pid:081575957 byr:1938 iyr:2012\n\niyr:2010 byr:1973\ncid:108\neyr:2026\npid:880191154 hcl:#888785 hgt:181cm\necl:brn\n\neyr:2021 iyr:2010 byr:1942 hcl:#7d3b0c ecl:hzl pid:886241926 hgt:171cm\n\ncid:53 byr:1993\npid:150cm eyr:2035\nhcl:#888785 hgt:153cm ecl:#128262 iyr:2021\n\necl:gry\npid:555911148\nhcl:#733820 eyr:2022 hgt:154cm iyr:2012\nbyr:1935 cid:338\n\nhcl:#b6652a\npid:833873846 iyr:2012\nhgt:167cm eyr:2023 byr:1984\n\neyr:2024\necl:blu byr:1955\nhcl:#b6652a pid:517975316 iyr:2010 hgt:166cm\n\npid:133785752\necl:blu\neyr:2024\nbyr:1973\niyr:2019 hcl:#fffffd\ncid:236 hgt:173cm\n\ncid:222\nbyr:2013 hcl:z eyr:2036 pid:7443967478 ecl:brn\niyr:2030 hgt:62cm\n\nhgt:193cm cid:259\nhcl:#18171d\necl:grn\nbyr:1995 pid:727880050 eyr:2030 iyr:2010\n\nhcl:#c0946f cid:275 eyr:1954 pid:772184635 ecl:#76add7 byr:2009 iyr:2018 hgt:151cm\n\necl:#52ed0f eyr:2033 hcl:#18171d pid:475397948\nbyr:1946 iyr:2028 hgt:178cm\n\niyr:2012 hgt:152cm\neyr:2027 byr:1923 ecl:brn\nhcl:#18171d pid:513722888 cid:171\n\niyr:2029\nhgt:111 hcl:z ecl:#33e3bc eyr:1930\nbyr:1934 pid:94036732\n\nhgt:154cm eyr:2024 hcl:#6b5442 iyr:2017\nbyr:1974\necl:amb pid:470968353 cid:345\n\nhgt:184cm hcl:#617375 eyr:2028\nbyr:1975 ecl:oth\niyr:2018 pid:735589126\n\ncid:261\nhcl:#cfa07d pid:213013397\nhgt:187cm\necl:gry iyr:2016\n\nhcl:#623a2f\necl:#34964b eyr:2009 pid:169cm byr:2028 hgt:169cm\niyr:2028\n\neyr:2029 iyr:2016\nbyr:1985\nhgt:192cm hcl:#602927 cid:167\necl:blu pid:620818510\n\neyr:2029\nbyr:1968\necl:blu\nhgt:183cm iyr:2011 pid:952376140 hcl:#efcc98\n\niyr:2020\nbyr:1981 pid:850136149 eyr:2028 hgt:159cm hcl:#7d3b0c\necl:brn\n\necl:brn pid:480452858 hgt:65in cid:340 eyr:2022\nbyr:1946\nhcl:#602927 iyr:2015\n\nhgt:172 hcl:z eyr:1958 iyr:1941 byr:2019 pid:389995951 ecl:dne\n\nbyr:2025 hcl:4c8dcd\nhgt:177in\necl:#55d635\ncid:197 pid:91192572\niyr:1921 eyr:2038\n\niyr:2027 pid:154cm\nhgt:185in byr:2012\neyr:2036 hcl:efd47d\necl:#64f98d\ncid:86\n\neyr:2029 pid:837224515 ecl:grn cid:231 hcl:#733820 iyr:2019\nhgt:159cm\nbyr:1977\n\npid:974518338 byr:1964 hcl:#cfa07d ecl:grn eyr:2030\nhgt:61in\niyr:2019\n\niyr:2019\nhgt:192in cid:94\neyr:1922\nbyr:1925 hcl:z ecl:utc pid:#081266\n\neyr:2027 iyr:2019 cid:328 byr:1961 hcl:#6b5442 ecl:blu hgt:177cm pid:235426720\n\nbyr:1959\neyr:2025\npid:890034625 ecl:oth\nhgt:62in cid:348 hcl:#733820\n\nhgt:161cm iyr:2018 pid:916160791 ecl:grn\nbyr:1951 hcl:#44d03a eyr:2025\n\nhgt:158cm byr:1942 iyr:2012 hcl:#602927\neyr:2026 ecl:gry pid:651231060\n\necl:hzl cid:340 pid:086942161 byr:1986 hcl:#a97842 iyr:2018\neyr:2028\nhgt:181cm\n\necl:blu\npid:278922687 cid:238 iyr:2018 hgt:153cm eyr:2027\nbyr:1965\nhcl:#733820\n\neyr:2023 cid:208 hgt:178cm hcl:#341e13 byr:1937 pid:290981079 iyr:2010 ecl:grn\n\nhcl:#888785\necl:amb\nbyr:1943 pid:559804716 eyr:2026 hgt:166cm\niyr:2019\n\npid:947831563\necl:gry\nbyr:1960 hcl:#341e13\niyr:2016 hgt:173cm eyr:2029\n\necl:blu iyr:2016 pid:724632073 hcl:#623a2f\neyr:2028 hgt:192cm byr:1958\n\nbyr:2021\neyr:2016 hcl:z iyr:1988 pid:65353943\necl:#bb553b\nhgt:125\n\nhcl:#efcc98 byr:1963 pid:290433211 eyr:2023 ecl:hzl\nhgt:172cm iyr:2013\n\niyr:2015 ecl:brn\nbyr:2023 hcl:#18171d\npid:325330679\nhgt:190in eyr:2023\n\npid:745674970 hgt:160cm eyr:2021 byr:1925 ecl:gry hcl:#341e13 iyr:2015\ncid:297\n\neyr:2021\npid:596411633\nbyr:1947 ecl:blu cid:191 hcl:#341e13 hgt:168cm iyr:2019\n\neyr:2030 pid:#902a6b iyr:1997 hcl:11f396 hgt:188cm byr:2025\necl:dne\n\neyr:2025\nbyr:2006\nhcl:#888785 ecl:hzl hgt:187cm\niyr:2012 pid:017702828\n\nbyr:1988 hcl:#18171d iyr:2019\npid:110591871\necl:hzl\nhgt:160cm\neyr:2029\n\necl:brn\nhcl:#c0946f iyr:2030 pid:264404022 byr:1984 hgt:59cm eyr:2040\n\npid:5973803069\nhcl:#cfa07d ecl:grt\nhgt:153cm eyr:2039 byr:1970\niyr:2025\n\nhcl:#fffffd\niyr:2022 byr:2026\nhgt:180 pid:82035145 eyr:2034 cid:118 ecl:utc\n\nhgt:186cm eyr:2026\necl:brn\niyr:2013 hcl:#8f4c9b pid:010260339 byr:1948\n\necl:amb hcl:#18171d iyr:2020 pid:259501214 byr:1978 hgt:193cm\ncid:263 eyr:2022\n\nhgt:161cm iyr:2015 byr:2014 eyr:2003\npid:708958872 ecl:grt\nhcl:f4a430\n\nhgt:170cm eyr:2021 pid:911638274 cid:110 byr:1963 ecl:blu\niyr:2015 hcl:1eda64\n\necl:oth byr:1949 hgt:174cm hcl:#18171d eyr:2022 iyr:2019\npid:305857230\n\necl:gry hcl:#a97842 pid:971971076 byr:2002 iyr:2019\nhgt:188cm\neyr:2022 cid:238\n\neyr:2027 pid:221315043 iyr:2010 hgt:159cm ecl:blu byr:1998 hcl:#6b5442\n\nhcl:#888785\nbyr:1926 eyr:2022 pid:433807814 ecl:grn\niyr:2010\nhgt:181cm\n\necl:grn hgt:164cm byr:1951 hcl:#18171d cid:75 pid:845508281 eyr:2021 iyr:2017\n\npid:#f59bc7\neyr:1987 hgt:191cm hcl:z byr:2024\niyr:1985\n\nhcl:#623a2f pid:497429747\nhgt:189cm\nbyr:1987\neyr:2027 iyr:2012 cid:95 ecl:hzl\n\nbyr:2000\nhgt:165cm\niyr:2017 pid:519443292 eyr:2029 cid:240 hcl:#a97842\necl:blu\n\ncid:67 pid:038299774\neyr:2023 iyr:2015 hgt:179cm byr:1941 hcl:#18171d ecl:amb\n\nbyr:2000\neyr:2025 ecl:oth iyr:2017\npid:334154607\nhcl:#fffffd hgt:173cm\n\nhcl:#888785 ecl:amb\ncid:131 iyr:2018 byr:1996 eyr:2026\nhgt:180cm pid:709543988\n\niyr:1988\npid:263277424\nhcl:ee8912 byr:1942 ecl:gry eyr:2040 hgt:161cm\n\neyr:2020 byr:1966 iyr:2020 hgt:169cm pid:611918000\nhcl:#7d3b0c ecl:hzl\n\nhgt:164cm ecl:brn\niyr:2015 pid:192054454 hcl:#6b5442 byr:1987 eyr:2022\n\nbyr:1952\necl:zzz\npid:215953654\neyr:2021 hcl:#efcc98 hgt:153cm iyr:2026\n\nhgt:167cm\nhcl:#b6652a pid:847614726\neyr:2022 ecl:gry byr:1990 iyr:2015\n\nhgt:185cm ecl:oth iyr:2012\nbyr:1933\ncid:250\npid:038674023\nhcl:#c0946f\n\npid:613273980 hcl:#a97842\necl:oth byr:1924 hgt:179cm\neyr:2027 iyr:1950\n\nhcl:#cfa07d byr:2018 hgt:190cm pid:64530329\necl:brn\niyr:2024\n\nhcl:z hgt:70cm pid:18807747\ncid:284 byr:2023\neyr:2035 ecl:#4a1501\niyr:1954\n\niyr:2016 hgt:152cm pid:886247173 byr:1940 hcl:#c0946f eyr:2027 ecl:oth cid:150\n\nhgt:152cm hcl:#48cfdf eyr:2025 cid:277\necl:oth pid:246230621 byr:1932\niyr:2020\n\necl:amb pid:871180042\ncid:117 hcl:#602927 iyr:2011 hgt:152cm\neyr:2030 byr:1999\n\neyr:2024 ecl:hzl hgt:171cm\nbyr:1934 pid:356408125 iyr:2019 hcl:#b6652a\ncid:169\n\neyr:2023\nhcl:#7d3b0c\nbyr:1934 hgt:67in ecl:oth pid:191785527\ncid:117 iyr:2016\n\niyr:2029\nhcl:#602927 eyr:2022 byr:1931 ecl:oth hgt:192cm\npid:231475143\n\necl:grn iyr:2014 cid:250 hcl:#b6652a byr:1970 pid:675238417 hgt:162cm\neyr:2026\n\necl:brn\nhcl:#623a2f eyr:2021 pid:293293433 hgt:158 byr:1977 iyr:2019\n\necl:oth hcl:#ceb3a1 pid:013111996 eyr:2023 hgt:180cm byr:1976 cid:224\n\nhgt:61cm\neyr:2027 ecl:amb pid:181cm iyr:1932\nbyr:1974\nhcl:#18171d\n\nbyr:1968 hgt:167cm\nhcl:#a97842 eyr:2022 iyr:2018 ecl:hzl pid:940968694\n\niyr:1943\nhgt:96\ncid:229\nhcl:z eyr:1990 byr:2007 pid:#25aa73\necl:#74592e\n\nhgt:182cm iyr:2018 ecl:hzl eyr:2029 byr:1946 pid:602345030\nhcl:#ceb3a1\n\npid:750306036 eyr:2020 hgt:181in ecl:xry\niyr:2011 hcl:z byr:1971 cid:71\n\npid:183825747 iyr:2019 hcl:#6b5442\nbyr:1974\nhgt:180cm eyr:2028\necl:amb\n\necl:brn cid:200 pid:576495225\nbyr:1924\nhcl:#efcc98 eyr:2022 iyr:2017 hgt:185cm\n\niyr:2020 hgt:167cm byr:1965 ecl:brn hcl:#888785\neyr:2028 pid:752062953\n\nbyr:2026\nhcl:z\neyr:2020\necl:#b4ec74 pid:187cm iyr:1974\ncid:326 hgt:150cm\n\nbyr:1996 pid:507323629\niyr:2015 cid:347 eyr:2026 hcl:#efcc98\necl:amb hgt:157cm\n\nbyr:2017 pid:456780590 hcl:#888785 eyr:1966 ecl:amb iyr:2023 cid:187 hgt:62cm\n\necl:hzl iyr:2015 hcl:#6b5442 hgt:152cm eyr:2028 byr:1982 pid:003269467\n\niyr:2017 eyr:2026\necl:blu cid:70 hcl:#7d3b0c\nbyr:1966 pid:160330947 hgt:189cm\n\niyr:2010 ecl:amb\nhgt:164cm eyr:2029 byr:1963\npid:596606374 hcl:#efcc98\n\nhcl:#fffffd cid:277 pid:102326370 hgt:154cm eyr:2026 iyr:2012 byr:1968\necl:hzl\n\necl:oth pid:477189554 hcl:#6b5442 eyr:2022 byr:1948 hgt:74in cid:181\niyr:2016\n\nhgt:169cm hcl:#d7bc93\ncid:344 ecl:oth\npid:#09c55d iyr:2017\neyr:2030 byr:1928\n\nhcl:5d02ff ecl:#ca7901 iyr:1959 byr:2006 eyr:2022\nhgt:164in\npid:#d6cdfd\n\necl:amb pid:5739190196 eyr:2021 hgt:157in hcl:#efcc98 byr:2018 iyr:2028\n\nbyr:1995 ecl:hzl\niyr:2017\nhcl:#a97842 pid:917039291 eyr:2026 hgt:175cm\n\niyr:2017 pid:756519868\nhcl:#623a2f\neyr:2028\nhgt:158cm\necl:amb byr:1957\n\niyr:2012\nhgt:158cm\nbyr:2014 pid:973021666 hcl:f04766 eyr:2035 ecl:utc\n\necl:blu\nbyr:1989 eyr:2022\npid:520765501\ncid:200 hgt:193cm hcl:#a97842 iyr:2011\n\nbyr:1959\necl:blu hcl:#733820 cid:284 hgt:162cm\neyr:2022 pid:751629408 iyr:2016\n\nbyr:1978 cid:301\necl:oth hgt:67cm hcl:#888785\neyr:2040 iyr:2025 pid:26038514\n\niyr:2020 byr:1974 hgt:163cm ecl:blu hcl:#7d3b0c eyr:2028 cid:99\n\nhcl:#a97842\nhgt:186cm\necl:grn byr:1969 pid:460360492 iyr:2011 eyr:2028\n\nbyr:2009\npid:489490924 eyr:2031\nhcl:cb5351 ecl:#083a25 hgt:164cm\n\niyr:2019\nhcl:3463cc ecl:amb pid:4089063078 eyr:2022 hgt:150cm\nbyr:2007\n\neyr:2028 hcl:#ceb3a1\nhgt:191cm iyr:2019 pid:737842199 ecl:blu cid:268 byr:1925\n\npid:868397851\nhcl:#efcc98 ecl:grn iyr:2017 eyr:2021 byr:1943\nhgt:179cm\n\nhcl:#623a2f byr:1987 eyr:2023 iyr:2019 hgt:152cm\npid:473569020\necl:grn\n\npid:953968630\nhgt:175cm\nbyr:1971 ecl:blu hcl:#623a2f iyr:2017 cid:336 eyr:2030\n\necl:grt hgt:74cm byr:2022 eyr:2024 pid:39114027\niyr:2026 hcl:4b5675\n\npid:#492988\neyr:2032 hgt:63cm iyr:2006\necl:#817211 byr:2019\n\npid:800367032 hcl:#341e13\necl:#765111 iyr:2012 byr:2006 hgt:166cm cid:291 eyr:2027\n\neyr:2021 iyr:2012 pid:876581393 ecl:amb hcl:#866857\nhgt:64in byr:1993\n\niyr:2017 byr:1996 ecl:hzl pid:038990744\neyr:2028\nhgt:177cm\nhcl:#c0946f\n\nhcl:#4214a6\neyr:2021\niyr:2019 cid:72 byr:1939\necl:hzl pid:783071912 hgt:187cm\n\neyr:2020 hgt:158cm\npid:274060737 cid:277\niyr:2015 hcl:#bf9b5e byr:1950 ecl:brn\n\nbyr:1921 hcl:#7d3b0c cid:329 hgt:155cm eyr:2030 pid:718399669 iyr:2011 ecl:brn\n\ncid:147 eyr:2021 hgt:167cm iyr:2010 ecl:grn byr:1975 hcl:#6b5442\npid:285479783\n\nhgt:187cm\nbyr:2004 eyr:2025 hcl:bb331b\npid:851189955 iyr:2016\necl:amb\n\nhcl:#94007d pid:361561551 byr:1927 eyr:2026 iyr:2020\necl:gry hgt:158cm\n\nbyr:1993 pid:#24c4af iyr:2023 hgt:175cm eyr:2028\nhcl:z ecl:hzl cid:308\n\nbyr:1985 hcl:#c0946f eyr:2034 hgt:172cm\ncid:300 iyr:2013 ecl:gry pid:389455676\n\neyr:2030 iyr:2017 byr:1956 hgt:178cm\npid:864401853 hcl:#6b5442\n\npid:836559549\niyr:2011\nhgt:167cm\necl:amb hcl:#c0946f\neyr:2026 byr:1981\n\npid:111085991 iyr:2011\necl:blu eyr:2026 cid:311\nbyr:1920 hgt:182cm hcl:#602927\n\necl:oth pid:284436132\nbyr:1929 cid:121\neyr:2027\niyr:2010\nhgt:75in\nhcl:#6b5442\n\nbyr:1987\nhcl:#7d3b0c iyr:2018 hgt:180cm\necl:blu eyr:2029 pid:878348021\n\nhgt:183cm cid:98\nbyr:1953 hcl:#866857 eyr:2021 iyr:2012 pid:158898193\n\neyr:2030 pid:039638764 ecl:hzl hgt:190cm byr:1926\ncid:294 hcl:#b6652a iyr:2017\n"

input_test1 = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"

data Passport = Passport { byr :: String -- Birth Year
                         , iyr :: String -- Issue Year
                         , eyr :: String -- Expiration Year
                         , hgt :: String -- Height
                         , hcl :: String -- Hair Color
                         , ecl :: String -- Eye Color
                         , pid :: String -- Passport ID
                         , cid :: String } deriving (Show) -- Country ID

-- Define non-Nothing 'default values' in advance
-- to avoid 'Fields of ‘Passport’ not initialised' warnings
-- Source: https://wiki.haskell.org/Default_values_in_records
passportTemplate = Passport { byr = "", iyr = "", eyr = "", hgt = "", hcl = "", ecl = "", pid = "", cid = "" }

parse_input :: String -> [String]
parse_input str = do
  -- Blank lines in the input delimit passports
  splitStr "\n\n" str

-- Split a string on given sub-string
-- Source: https://gist.github.com/kevinadi/da8fbd30b3e03300ce56
splitStr :: Eq a => [a] -> [a] -> [[a]]
splitStr sub str = split' sub str [] []
    where
    split' _   []  subacc acc = reverse (reverse subacc:acc)
    split' sub str subacc acc
        | sub `isPrefixOf` str = split' sub (drop (length sub) str) [] (reverse subacc:acc)
        | otherwise            = split' sub (tail str) (head str:subacc) acc

-- Take "key:value" string and return tuple (key, value)
splitColon :: String -> (String, String)
splitColon str = do
  let parts = splitStr ":" str
  ((parts !! 0), (parts !! 1))

-- Lookup a key in association list, returning empty string if not found
-- Adapted from 'lookup' in Haskell base package;
-- http://hackage.haskell.org/package/base-4.8.1.0/docs/src/GHC.List.html#lookup
lookupOrEmptyString :: (Eq a) => a -> [(a,String)] -> String
lookupOrEmptyString _key [] = ""
lookupOrEmptyString key ((x,y):xys)
    | key == x = y
    | otherwise = lookupOrEmptyString key xys

-- Take key-value tuples and write into a Passport record
-- Adapted from https://stackoverflow.com/a/37859036/1323144
-- Note: We use an empty string fallback because the `Maybe` approach (using
-- vanilla `lookup` which returns `Maybe a`, incl perhaps `Nothing`) causes
-- a "Missing field in record construction" error
to_passport :: [(String, String)] -> Passport
to_passport kvs = passportTemplate { byr = (lookupOrEmptyString "byr" kvs)
                                   , iyr = (lookupOrEmptyString "iyr" kvs)
                                   , eyr = (lookupOrEmptyString "eyr" kvs)
                                   , hgt = (lookupOrEmptyString "hgt" kvs)
                                   , hcl = (lookupOrEmptyString "hcl" kvs)
                                   , ecl = (lookupOrEmptyString "ecl" kvs)
                                   , pid = (lookupOrEmptyString "pid" kvs)
                                   , cid = (lookupOrEmptyString "cid" kvs) }

extract_passport :: String -> [(String, String)]
extract_passport pp_string = do
  -- split string by space or newlines;
  let passport_parts_raw = words pp_string
  -- convert ["a:x", "b:y"] to [(a, x), (b, y)]
  map splitColon passport_parts_raw

build_passport :: String -> Passport
build_passport pp_string = do
  to_passport (extract_passport pp_string)

-- Birth Year - four digits; at least 1920 and at most 2002.
-- Adapted from: https://stackoverflow.com/a/49464930/1323144
valid_byr :: String -> Bool
valid_byr str = do
  length str == 4
    && (all (`elem` "0123456789") str)
    && (read str :: Int) >= 1920
    && (read str :: Int) <= 2002

-- Issue Year - four digits; at least 2010 and at most 2020.
valid_iyr :: String -> Bool
valid_iyr str = do
  length str == 4
    && (all (`elem` "0123456789") str)
    && (read str :: Int) >= 2010
    && (read str :: Int) <= 2020

-- Expiration Year - four digits; at least 2020 and at most 2030.
valid_eyr :: String -> Bool
valid_eyr str = do
  length str == 4
    && (all (`elem` "0123456789") str)
    && (read str :: Int) >= 2020
    && (read str :: Int) <= 2030

-- Height - a number followed by either cm or in:
-- If cm, the number must be at least 150 and at most 193.
-- If in, the number must be at least 59 and at most 76.
valid_hgt :: String -> Bool
valid_hgt str = do
  if length str <= 2
    then False
    else do
      let units = drop (length str -2) str
      let number_str = take (length str -2) str
      if (all (`elem` "0123456789") number_str) && (elem units ["in", "cm"])
        then do
          let number = read number_str :: Int
          if units == "cm"
            then number >= 150 && number <= 193
            else number >= 59 && number <= 76
        else False -- invalid number format or unit

-- Hair Color - a # followed by exactly six characters 0-9 or a-f.
valid_hcl :: String -> Bool
valid_hcl str = do
  length str == 7
    && str !! 0 == '#'
    && (all (`elem` "0123456789abcdef") (drop 1 str))

-- Eye Color - exactly one of: amb blu brn gry grn hzl oth.
valid_ecl :: String -> Bool
valid_ecl str = do
  let valid_ecl_values = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  elem str valid_ecl_values

-- Passport ID - a nine-digit number, including leading zeroes.
valid_pid :: String -> Bool
valid_pid str = do
  length str == 9
    && (all (`elem` "0123456789") str)

-- Country ID - ignored, whether missing or not.
valid_cid :: String -> Bool
valid_cid cid = do
  True

valid_passport :: Passport -> Bool
valid_passport passport = do
  -- Passport is valid if NO fields are empty, except cid (which we ignore)
  byr passport /= ""
  && iyr passport /= ""
  && eyr passport /= ""
  && hgt passport /= ""
  && hcl passport /= ""
  && ecl passport /= ""
  && pid passport /= ""

valid_passport_v2 :: Passport -> Bool
valid_passport_v2 passport = do
  valid_byr(byr passport)
  && valid_iyr(iyr passport)
  && valid_eyr(eyr passport)
  && valid_hgt(hgt passport)
  && valid_hcl(hcl passport)
  && valid_ecl(ecl passport)
  && valid_pid(pid passport)
  && valid_cid(cid passport)

valid_passport_string :: String -> Bool
valid_passport_string pp_string = do
  let passport = build_passport pp_string
  valid_passport passport

valid_passport_string_v2 :: String -> Bool
valid_passport_string_v2 pp_string = do
  let passport = build_passport pp_string
  valid_passport_v2 passport

main = do
  putStrLn "AoC 2020 - Day 4"

  putStrLn "-- Part 1 Test"
  let test_passports = parse_input input_test1
  putStrLn $ "Total valid test passports: " ++ show (length (filter valid_passport_string test_passports))

  putStrLn "-- Part 1 Solution"
  let passports = parse_input input
  let num_passports = length passports
  let valid_passport_count = length (filter valid_passport_string passports)
  putStrLn $ "Total valid passports: " ++ show valid_passport_count ++ " out of " ++ show num_passports

  putStrLn "-- Part 2 Tests"
  putStrLn $ "byr=2002 [VALID] => " ++ show (valid_byr "2002")
  putStrLn $ "byr=2003 [INVALID]=> " ++ show (valid_byr "2003")
  putStrLn $ "byr=hello [INVALID] => " ++ show (valid_byr "hello")
  putStrLn $ "byr=3 [INVALID] => " ++ show (valid_byr "3")
  putStrLn $ "hgt=60in [VALID] => " ++ show (valid_hgt "60in")
  putStrLn $ "hgt=190cm [VALID] => " ++ show (valid_hgt "190cm")
  putStrLn $ "hgt=190in [INVALID] => " ++ show (valid_hgt "190in")
  putStrLn $ "hgt=190 [INVALID] => " ++ show (valid_hgt "190")
  putStrLn $ "hcl=#123abc [VALID] => " ++ show (valid_hcl "#123abc")
  putStrLn $ "hcl=#123abz [INVALID] => " ++ show (valid_hcl "#123abz")
  putStrLn $ "hcl=123abc [INVALID] => " ++ show (valid_hcl "123abc")
  putStrLn $ "ecl=brn [VALID] => " ++ show (valid_ecl "brn")
  putStrLn $ "ecl=wat [INVALID] => " ++ show (valid_ecl "wat")
  putStrLn $ "pid=000000001 [VALID] => " ++ show (valid_pid "000000001")
  putStrLn $ "pid=0123456789 [INVALID] => " ++ show (valid_pid "0123456789")

  putStrLn "-- Part 2 Solution"
  let valid_passport_count_v2 = length (filter valid_passport_string_v2 passports)
  putStrLn $ "Total valid passports: " ++ show valid_passport_count_v2 ++ " out of " ++ show num_passports
