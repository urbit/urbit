/-  asn1
/+  primitive-rsa, *pkcs, *test
=*  rsa  primitive-rsa
|%
++  test-rsapem
  ::  ex from https://stackoverflow.com/a/19855935
  =/  k1=key:rsa
    :-  [`@ux`187 `@ux`7]
    [~ `@ux`23 `@ux`17 `@ux`11]
  =/  kpem1=wain
    :~  '-----BEGIN RSA PRIVATE KEY-----'
        'MBwCAQACAgC7AgEHAgEXAgERAgELAgEHAgEDAgEO'
        '-----END RSA PRIVATE KEY-----'
    ==
  =/  k2=key:rsa
    :*  [n=`@ux`16.238.973.331.713.186.433 e=`@ux`65.537]
        ~
        d=`@ux`3.298.243.342.098.580.397
        p=`@ux`4.140.273.707
        q=`@ux`3.922.198.019
    ==
  :: openssl genrsa -out private.pem 64
  =/  kpem2=wain
    :~  '-----BEGIN RSA PRIVATE KEY-----'
        'MEACAQACCQDhXGw1Gc5agQIDAQABAggtxbbYRJVDrQIFAPbHkCsCBQDpx/4DAgUA'
        '23X55QIFAIpPROsCBQC56nYF'
        '-----END RSA PRIVATE KEY-----'
    ==
  :: openssl genrsa -out private.pem 2048
  =/  kpem3=wain
    :~  '-----BEGIN RSA PRIVATE KEY-----'
        'MIIEowIBAAKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5'
        'qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdX'
        'CCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3'
        'HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNe'
        'WlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6'
        'BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQABAoIBAH0q7GGisj4TIziy'
        '6k1lzwXMuaO4iwO+gokIeU5UessIgTSfpK1G73CnZaPstDPF1r/lncHfxZfTQuij'
        'WOHsO7kt+x5+R0ebDd0ZGVA45fsrPrCUR2XRZmDRECuOfTJGA13G7F1B0kJUbfIb'
        'gAGYIK8x236WNyIrntk804SGpTgstCsZ51rK5GL6diZVQbeU806oP1Zhx/ye//NR'
        'mS5G0iil//H41pV5WGomOX0mq9/HYBZqCncqzLki6FFdmXykjz8snvXUR40S8B+a'
        '0F/LN+549PSe2dp9h0Hx4HCJOsL9CyCQimqqqE8KPQ4BUz8q3+Mhx1xEyaxIlNH9'
        'ECgo1CECgYEA+mi7vQRzstYJerbhCtaeFrOR/n8Dft7FyFN+5IV7H2omy6gf0zr1'
        'GWjmph5R0sMPgL8uVRGANUrkuZZuCr35iY6zQpdCFB4D9t+zbTvTmrxt2oVaE16/'
        'dIJ6b8cHzR2QrEh8uw5/rEKzWBCHNS8FvXHPvXvnacTZ5LZRK0ssshECgYEA3xGQ'
        'nDlmRwyVto/1DQMLnjIMazQ719qtCO/pf4BHeqcDYnIwYb5zLBj2nPV8D9pqM1pG'
        'OVuOgcC9IimrbHeeGwp1iSTH4AvxDIj6Iyrmbz2db3lGdHVk9xLvTiYzn2KK2sYx'
        'mFl3DRBFutFQ2YxddqHbE3Ds96Y/uRXhqj7I16cCgYEA1AVNwHM+i1OS3yZtUUH6'
        'xPnySWu9x/RTvpSDwnYKk8TLaHDH0Y//6y3Y7RqK6Utjmv1E+54/0d/B3imyrsG/'
        'wWrj+SQdPO9VJ/is8XZQapnU4cs7Q19b+AhqJq58un2n+1e81J0oGPC47X3BHZTc'
        '5VSyMpvwiqu0WmTMQT37cCECgYACMEbt8XY6bjotz13FIemERNNwXdPUe1XFR61P'
        'ze9lmavj1GD7JIY2wYvx4Eq2URtHo7QarfZI+Z4hbq065DWN6F1c2hqH7TYRPGrP'
        '24TlRIJ97H+vdtNlxS7J4oARKUNZgCZOa1pKq4UznwgfCkyEdHQUzb/VcjEf3MIZ'
        'DIKl8wKBgBrsIjiDvpkfnpmQ7fehEJIi+V4SGskLxFH3ZTvngFFoYry3dL5gQ6mF'
        'sDfrn4igIcEy6bMpJQ3lbwStyzcWZLMJgdI23FTlPXTEG7PclZSuxBpQpvg3MiVO'
        'zqVTrhnY+TemcScSx5O6f32aDfOUWWCzmw/gzvJxUYlJqjqd7dlT'
        '-----END RSA PRIVATE KEY-----'
    ==
  =/  kpem3-pub=wain
    :~  '-----BEGIN RSA PUBLIC KEY-----'
        'MIIBCgKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5qxx2'
        'IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdXCCSG'
        '7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3HhgS'
        'iNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNeWlqT'
        '9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6BoIG'
        'I5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQAB'
        '-----END RSA PUBLIC KEY-----'
    ==
  =/  k3=key:rsa
    (need (ring:de:pem:pkcs1 kpem3))
  =/  k3-pub=key:rsa
    (need (pass:de:pem:pkcs1 kpem3-pub))
  ;:  weld
    %+  expect-eq
      !>  kpem1
      !>  (ring:en:pem:pkcs1 k1)
  ::
    %+  expect-eq
      !>  k1
      !>  (need (ring:de:pem:pkcs1 kpem1))
  ::
    %+  expect-eq
      !>  kpem2
      !>  (ring:en:pem:pkcs1 k2)
  ::
    %+  expect-eq
      !>  k2
      !>  (need (ring:de:pem:pkcs1 kpem2))
  ::
    %+  expect-eq
      !>  kpem3
      !>  (ring:en:pem:pkcs1 k3)
  ::
    %+  expect-eq
      !>  kpem3-pub
      !>  (pass:en:pem:pkcs1 k3)
  ::
    %+  expect-eq
      !>  k3-pub
      !>  `key:rsa`[pub.k3 ~]
  ==
::
++  test-rsa-pkcs8
  =/  kpem=wain
    :~  '-----BEGIN RSA PRIVATE KEY-----'
        'MIIEowIBAAKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5'
        'qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdX'
        'CCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3'
        'HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNe'
        'WlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6'
        'BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQABAoIBAH0q7GGisj4TIziy'
        '6k1lzwXMuaO4iwO+gokIeU5UessIgTSfpK1G73CnZaPstDPF1r/lncHfxZfTQuij'
        'WOHsO7kt+x5+R0ebDd0ZGVA45fsrPrCUR2XRZmDRECuOfTJGA13G7F1B0kJUbfIb'
        'gAGYIK8x236WNyIrntk804SGpTgstCsZ51rK5GL6diZVQbeU806oP1Zhx/ye//NR'
        'mS5G0iil//H41pV5WGomOX0mq9/HYBZqCncqzLki6FFdmXykjz8snvXUR40S8B+a'
        '0F/LN+549PSe2dp9h0Hx4HCJOsL9CyCQimqqqE8KPQ4BUz8q3+Mhx1xEyaxIlNH9'
        'ECgo1CECgYEA+mi7vQRzstYJerbhCtaeFrOR/n8Dft7FyFN+5IV7H2omy6gf0zr1'
        'GWjmph5R0sMPgL8uVRGANUrkuZZuCr35iY6zQpdCFB4D9t+zbTvTmrxt2oVaE16/'
        'dIJ6b8cHzR2QrEh8uw5/rEKzWBCHNS8FvXHPvXvnacTZ5LZRK0ssshECgYEA3xGQ'
        'nDlmRwyVto/1DQMLnjIMazQ719qtCO/pf4BHeqcDYnIwYb5zLBj2nPV8D9pqM1pG'
        'OVuOgcC9IimrbHeeGwp1iSTH4AvxDIj6Iyrmbz2db3lGdHVk9xLvTiYzn2KK2sYx'
        'mFl3DRBFutFQ2YxddqHbE3Ds96Y/uRXhqj7I16cCgYEA1AVNwHM+i1OS3yZtUUH6'
        'xPnySWu9x/RTvpSDwnYKk8TLaHDH0Y//6y3Y7RqK6Utjmv1E+54/0d/B3imyrsG/'
        'wWrj+SQdPO9VJ/is8XZQapnU4cs7Q19b+AhqJq58un2n+1e81J0oGPC47X3BHZTc'
        '5VSyMpvwiqu0WmTMQT37cCECgYACMEbt8XY6bjotz13FIemERNNwXdPUe1XFR61P'
        'ze9lmavj1GD7JIY2wYvx4Eq2URtHo7QarfZI+Z4hbq065DWN6F1c2hqH7TYRPGrP'
        '24TlRIJ97H+vdtNlxS7J4oARKUNZgCZOa1pKq4UznwgfCkyEdHQUzb/VcjEf3MIZ'
        'DIKl8wKBgBrsIjiDvpkfnpmQ7fehEJIi+V4SGskLxFH3ZTvngFFoYry3dL5gQ6mF'
        'sDfrn4igIcEy6bMpJQ3lbwStyzcWZLMJgdI23FTlPXTEG7PclZSuxBpQpvg3MiVO'
        'zqVTrhnY+TemcScSx5O6f32aDfOUWWCzmw/gzvJxUYlJqjqd7dlT'
        '-----END RSA PRIVATE KEY-----'
    ==
  =/  pub=wain
    :~  '-----BEGIN PUBLIC KEY-----'
        'MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA2jJp8dgAKy5cSzDE4D+a'
        'UbKZsQoMhIWI2IFlE+AO0GCBMig5qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8Q'
        'IKRvkZaIwnA3Lz5UUrxgh96sezdXCCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3'
        'kneo5aAKMIPyOTzcY590UTc+luQ3HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxN'
        'fBgXbV+C7/gO8mFxpkafhmgkIGNeWlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGv'
        'xvBMp3xUASlzYHiDntcB5MiOPRW6BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxw'
        'FwIDAQAB'
        '-----END PUBLIC KEY-----'
    ==
  =/  pri=wain
    :~  '-----BEGIN PRIVATE KEY-----'
        'MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQDaMmnx2AArLlxL'
        'MMTgP5pRspmxCgyEhYjYgWUT4A7QYIEyKDmrHHYggA9UhxKLl+M4u1Mee4hljD6z'
        'Tqp5vxAgpG+RlojCcDcvPlRSvGCH3qx7N1cIJIbsWd0gWyRxP7MbTQkv589F2U+O'
        '3VWDZveSd6jloAowg/I5PNxjn3RRNz6W5DceGBKI0XeflhCHbh3eRLZg5ShJdDXf'
        '5hGWPE18GBdtX4Lv+A7yYXGmRp+GaCQgY15aWpP2gAhFr6A7HXe48CD4lv/yU9X1'
        'sZoWoa/G8EynfFQBKXNgeIOe1wHkyI49FboGggYjmoMVJhFkGTL1ysTtYBiCq0+2'
        'DbXO/HAXAgMBAAECggEAfSrsYaKyPhMjOLLqTWXPBcy5o7iLA76CiQh5TlR6ywiB'
        'NJ+krUbvcKdlo+y0M8XWv+Wdwd/Fl9NC6KNY4ew7uS37Hn5HR5sN3RkZUDjl+ys+'
        'sJRHZdFmYNEQK459MkYDXcbsXUHSQlRt8huAAZggrzHbfpY3Iiue2TzThIalOCy0'
        'KxnnWsrkYvp2JlVBt5TzTqg/VmHH/J7/81GZLkbSKKX/8fjWlXlYaiY5fSar38dg'
        'FmoKdyrMuSLoUV2ZfKSPPyye9dRHjRLwH5rQX8s37nj09J7Z2n2HQfHgcIk6wv0L'
        'IJCKaqqoTwo9DgFTPyrf4yHHXETJrEiU0f0QKCjUIQKBgQD6aLu9BHOy1gl6tuEK'
        '1p4Ws5H+fwN+3sXIU37khXsfaibLqB/TOvUZaOamHlHSww+Avy5VEYA1SuS5lm4K'
        'vfmJjrNCl0IUHgP237NtO9OavG3ahVoTXr90gnpvxwfNHZCsSHy7Dn+sQrNYEIc1'
        'LwW9cc+9e+dpxNnktlErSyyyEQKBgQDfEZCcOWZHDJW2j/UNAwueMgxrNDvX2q0I'
        '7+l/gEd6pwNicjBhvnMsGPac9XwP2mozWkY5W46BwL0iKatsd54bCnWJJMfgC/EM'
        'iPojKuZvPZ1veUZ0dWT3Eu9OJjOfYoraxjGYWXcNEEW60VDZjF12odsTcOz3pj+5'
        'FeGqPsjXpwKBgQDUBU3Acz6LU5LfJm1RQfrE+fJJa73H9FO+lIPCdgqTxMtocMfR'
        'j//rLdjtGorpS2Oa/UT7nj/R38HeKbKuwb/BauP5JB0871Un+KzxdlBqmdThyztD'
        'X1v4CGomrny6faf7V7zUnSgY8LjtfcEdlNzlVLIym/CKq7RaZMxBPftwIQKBgAIw'
        'Ru3xdjpuOi3PXcUh6YRE03Bd09R7VcVHrU/N72WZq+PUYPskhjbBi/HgSrZRG0ej'
        'tBqt9kj5niFurTrkNY3oXVzaGoftNhE8as/bhOVEgn3sf69202XFLsnigBEpQ1mA'
        'Jk5rWkqrhTOfCB8KTIR0dBTNv9VyMR/cwhkMgqXzAoGAGuwiOIO+mR+emZDt96EQ'
        'kiL5XhIayQvEUfdlO+eAUWhivLd0vmBDqYWwN+ufiKAhwTLpsyklDeVvBK3LNxZk'
        'swmB0jbcVOU9dMQbs9yVlK7EGlCm+DcyJU7OpVOuGdj5N6ZxJxLHk7p/fZoN85RZ'
        'YLObD+DO8nFRiUmqOp3t2VM='
        '-----END PRIVATE KEY-----'
    ==
  =/  k=key:rsa
    (need (ring:de:pem:pkcs1 kpem))
  ;:  weld
    %+  expect-eq
      !>  pub
      !>  (pass:en:pem:pkcs8 k)
  ::
    %+  expect-eq
      !>  `k(sek ~)
      !>  (pass:de:pem:pkcs8 pub)
  ::
    %+  expect-eq
      !>  pri
      !>  (ring:en:pem:pkcs8 k)
  ::
    %+  expect-eq
      !>  `k
      !>  (ring:de:pem:pkcs8 pri)
  ==
::
++  test-rsa-pem-zero
  :: intentional bad values to test significant trailing zeros
  =/  k=key:rsa  [[n=(bex 16) e=(bex 16)] ~]
  :: pkcs1
  =/  kpem=wain
    :~  '-----BEGIN RSA PUBLIC KEY-----'
        'MAoCAwEAAAIDAQAA'
        '-----END RSA PUBLIC KEY-----'
    ==
  :: pkcs8
  =/  kpem2=wain
  :~  '-----BEGIN PUBLIC KEY-----'
      'MB4wDQYJKoZIhvcNAQEBBQADDQAwCgIDAQAAAgMBAAA='
      '-----END PUBLIC KEY-----'
  ==
  ;:  weld
    %+  expect-eq
      !>  kpem
      !>  (pass:en:pem:pkcs1 k)
  ::
    %+  expect-eq
      !>  `k
      !>  (pass:de:pem:pkcs1 kpem)
  ::
    %+  expect-eq
      !>  kpem2
      !>  (pass:en:pem:pkcs8 k)
  ::
    %+  expect-eq
      !>  `k
      !>  (pass:de:pem:pkcs8 kpem2)
  ==
::
++  test-rs256
  ::  ex from https://stackoverflow.com/a/41448118
  =/  k1=key:rsa
    :*  :-  n=0xc231.1fc5.fa31.d333.a409.bb4c.e95b.20d2.1cfc.e375.3871.7256.53a2.
          8425.af6d.e97d.f202.0b23.633f.458d.f12a.6362.7121.bff4.e23c.e578.
          7e07.7898.0578.61d1.ae60.ac2f
        e=0x1.0001
        ~
        d=0xd91.6719.eb10.3e24.768a.a386.8d2b.6bd0.a26b.dcec.9cc3.f86c.25ad.
          ce33.dfdc.fb1a.4d50.3e07.3d7f.f5fd.748e.43f8.df02.a60e.d730.5314.
          3e59.1e70.8df7.2c27.93e2.2b69
        p=0xf7ef.37e6.7fa6.685a.c178.8b01.cf38.da20.ca4b.de5d.8b01.a71b.d28c.
          65b4.09c3.6e4d
        q=0xc882.5760.3fb8.a5e2.5e9d.db55.3a73.b647.a3ec.a6e9.abc6.c440.dbc7.
          05f8.2ed4.da6b
    ==
  =/  inp1  0x302.0101
  =/  exp1
    0x575c.8a41.09ed.6ea2.a708.6338.d150.a5bb.8205.142e.7785.47b5.0cc6.0198.
      6807.0243.bf49.de7c.6039.0160.e392.faca.18f4.a05d.3a7a.88a4.de86.dd99.
      f030.eb4a.a755.d7ce
  =/  emsa1
    0x1.ffff.ffff.ffff.ffff.ffff.0030.3130.0d06.0960.8648.0165.0304.0201.0500.
        0420.9184.abd2.bb31.8731.d717.e972.0572.40ea.e26c.ca20.2a8d.35db.e9d2.
        176f.5268.86a0
  =/  kpem2=wain
    :~  '-----BEGIN RSA PRIVATE KEY-----'
        'MIIEowIBAAKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5'
        'qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdX'
        'CCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3'
        'HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNe'
        'WlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6'
        'BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQABAoIBAH0q7GGisj4TIziy'
        '6k1lzwXMuaO4iwO+gokIeU5UessIgTSfpK1G73CnZaPstDPF1r/lncHfxZfTQuij'
        'WOHsO7kt+x5+R0ebDd0ZGVA45fsrPrCUR2XRZmDRECuOfTJGA13G7F1B0kJUbfIb'
        'gAGYIK8x236WNyIrntk804SGpTgstCsZ51rK5GL6diZVQbeU806oP1Zhx/ye//NR'
        'mS5G0iil//H41pV5WGomOX0mq9/HYBZqCncqzLki6FFdmXykjz8snvXUR40S8B+a'
        '0F/LN+549PSe2dp9h0Hx4HCJOsL9CyCQimqqqE8KPQ4BUz8q3+Mhx1xEyaxIlNH9'
        'ECgo1CECgYEA+mi7vQRzstYJerbhCtaeFrOR/n8Dft7FyFN+5IV7H2omy6gf0zr1'
        'GWjmph5R0sMPgL8uVRGANUrkuZZuCr35iY6zQpdCFB4D9t+zbTvTmrxt2oVaE16/'
        'dIJ6b8cHzR2QrEh8uw5/rEKzWBCHNS8FvXHPvXvnacTZ5LZRK0ssshECgYEA3xGQ'
        'nDlmRwyVto/1DQMLnjIMazQ719qtCO/pf4BHeqcDYnIwYb5zLBj2nPV8D9pqM1pG'
        'OVuOgcC9IimrbHeeGwp1iSTH4AvxDIj6Iyrmbz2db3lGdHVk9xLvTiYzn2KK2sYx'
        'mFl3DRBFutFQ2YxddqHbE3Ds96Y/uRXhqj7I16cCgYEA1AVNwHM+i1OS3yZtUUH6'
        'xPnySWu9x/RTvpSDwnYKk8TLaHDH0Y//6y3Y7RqK6Utjmv1E+54/0d/B3imyrsG/'
        'wWrj+SQdPO9VJ/is8XZQapnU4cs7Q19b+AhqJq58un2n+1e81J0oGPC47X3BHZTc'
        '5VSyMpvwiqu0WmTMQT37cCECgYACMEbt8XY6bjotz13FIemERNNwXdPUe1XFR61P'
        'ze9lmavj1GD7JIY2wYvx4Eq2URtHo7QarfZI+Z4hbq065DWN6F1c2hqH7TYRPGrP'
        '24TlRIJ97H+vdtNlxS7J4oARKUNZgCZOa1pKq4UznwgfCkyEdHQUzb/VcjEf3MIZ'
        'DIKl8wKBgBrsIjiDvpkfnpmQ7fehEJIi+V4SGskLxFH3ZTvngFFoYry3dL5gQ6mF'
        'sDfrn4igIcEy6bMpJQ3lbwStyzcWZLMJgdI23FTlPXTEG7PclZSuxBpQpvg3MiVO'
        'zqVTrhnY+TemcScSx5O6f32aDfOUWWCzmw/gzvJxUYlJqjqd7dlT'
        '-----END RSA PRIVATE KEY-----'
    ==
  =/  k2=key:rsa
    (need (ring:de:pem:pkcs1 kpem2))
  =/  inp2=cord  'hello\0a'
  =/  exp2=@ux
    0x2920.bba3.cb38.bca6.3768.6345.c95e.0717.81bf.6c61.4006.6070.a7b5.e609.
      f3b4.7f48.878b.d1f8.1882.8852.1db6.b6b5.a5fd.c23b.e764.b910.5a3f.fda9.
      9d3a.e8bd.060a.ac06.58f1.487a.b50d.dee2.e161.0b74.4d3b.e6e3.7004.c721.
      4f32.5c95.ce68.a008.b1e9.788b.375f.d389.0fa4.4012.c07a.8319.a183.02d5.
      e2b8.10df.6ff7.f64a.6b85.3c7d.de80.19cf.ab6d.e588.40cb.0ea4.c436.8d8b.
      47f7.cce6.b9bf.097d.3275.c128.147a.628d.2b7c.3912.3950.ef68.87b2.180d.
      ba01.3b05.285d.3dfd.09ee.2f38.3111.9e4c.92c6.bf66.a91b.5762.3cdf.f8b7.
      8281.81a2.8324.5330.43c1.035a.56c3.71b8.eb85.e660.c3a4.28b4.8af7.c16f.
      7d7d.87cc.036d.aeb2.c757.30f5.f194.c90d.6bb4.5e5c.f95f.8e28.0fbc.5fb4.
      b21a.e6fe
  =/  exp2b64=cord
    %+  rap  3
    :~  'KSC7o8s4vKY3aGNFyV4HF4G/bGFABmBwp7XmCfO0f0iHi9H4GIKIUh22trWl/cI752S5'
        'EFo//amdOui9BgqsBljxSHq1Dd7i4WELdE075uNwBMchTzJclc5ooAix6XiLN1/TiQ+k'
        'QBLAeoMZoYMC1eK4EN9v9/ZKa4U8fd6AGc+rbeWIQMsOpMQ2jYtH98zmub8JfTJ1wSgU'
        'emKNK3w5EjlQ72iHshgNugE7BShdPf0J7i84MRGeTJLGv2apG1diPN/4t4KBgaKDJFMw'
        'Q8EDWlbDcbjrheZgw6QotIr3wW99fYfMA22ussdXMPXxlMkNa7ReXPlfjigPvF+0shrm'
        '/g=='
    ==
  =/  sig=@ux  (~(sign rs256 k2) (met 3 inp2) inp2)
  ;:  weld
    %+  expect-eq
      !>  exp1
      !>  (~(sign rs256 k1) (met 3 inp1) inp1)
  ::
    %+  expect-eq
      !>  &
      !>  (~(verify rs256 k1) exp1 (met 3 inp1) inp1)
  ::
    %+  expect-eq
      !>  emsa1
      !>  `@ux`(~(emsa rs256 k1) (met 3 inp1) inp1)
  ::
    %+  expect-eq
      !>  &
      !>  (~(verify rs256 k2) sig (met 3 inp2) inp2)
  ::
    %+  expect-eq
      !>  exp2
      !>  sig
  ::
    :: save kpem2 to private.pem
    :: echo "hello" | openssl dgst -sha256 -sign private.pem | base64
    %+  expect-eq
      !>  exp2b64
      !>  (en:base64 (met 3 sig) (swp 3 sig))
  ==
::
++  test-csr
  =/  kpem=wain
    :~  '-----BEGIN RSA PRIVATE KEY-----'
        'MIIEowIBAAKCAQEA2jJp8dgAKy5cSzDE4D+aUbKZsQoMhIWI2IFlE+AO0GCBMig5'
        'qxx2IIAPVIcSi5fjOLtTHnuIZYw+s06qeb8QIKRvkZaIwnA3Lz5UUrxgh96sezdX'
        'CCSG7FndIFskcT+zG00JL+fPRdlPjt1Vg2b3kneo5aAKMIPyOTzcY590UTc+luQ3'
        'HhgSiNF3n5YQh24d3kS2YOUoSXQ13+YRljxNfBgXbV+C7/gO8mFxpkafhmgkIGNe'
        'WlqT9oAIRa+gOx13uPAg+Jb/8lPV9bGaFqGvxvBMp3xUASlzYHiDntcB5MiOPRW6'
        'BoIGI5qDFSYRZBky9crE7WAYgqtPtg21zvxwFwIDAQABAoIBAH0q7GGisj4TIziy'
        '6k1lzwXMuaO4iwO+gokIeU5UessIgTSfpK1G73CnZaPstDPF1r/lncHfxZfTQuij'
        'WOHsO7kt+x5+R0ebDd0ZGVA45fsrPrCUR2XRZmDRECuOfTJGA13G7F1B0kJUbfIb'
        'gAGYIK8x236WNyIrntk804SGpTgstCsZ51rK5GL6diZVQbeU806oP1Zhx/ye//NR'
        'mS5G0iil//H41pV5WGomOX0mq9/HYBZqCncqzLki6FFdmXykjz8snvXUR40S8B+a'
        '0F/LN+549PSe2dp9h0Hx4HCJOsL9CyCQimqqqE8KPQ4BUz8q3+Mhx1xEyaxIlNH9'
        'ECgo1CECgYEA+mi7vQRzstYJerbhCtaeFrOR/n8Dft7FyFN+5IV7H2omy6gf0zr1'
        'GWjmph5R0sMPgL8uVRGANUrkuZZuCr35iY6zQpdCFB4D9t+zbTvTmrxt2oVaE16/'
        'dIJ6b8cHzR2QrEh8uw5/rEKzWBCHNS8FvXHPvXvnacTZ5LZRK0ssshECgYEA3xGQ'
        'nDlmRwyVto/1DQMLnjIMazQ719qtCO/pf4BHeqcDYnIwYb5zLBj2nPV8D9pqM1pG'
        'OVuOgcC9IimrbHeeGwp1iSTH4AvxDIj6Iyrmbz2db3lGdHVk9xLvTiYzn2KK2sYx'
        'mFl3DRBFutFQ2YxddqHbE3Ds96Y/uRXhqj7I16cCgYEA1AVNwHM+i1OS3yZtUUH6'
        'xPnySWu9x/RTvpSDwnYKk8TLaHDH0Y//6y3Y7RqK6Utjmv1E+54/0d/B3imyrsG/'
        'wWrj+SQdPO9VJ/is8XZQapnU4cs7Q19b+AhqJq58un2n+1e81J0oGPC47X3BHZTc'
        '5VSyMpvwiqu0WmTMQT37cCECgYACMEbt8XY6bjotz13FIemERNNwXdPUe1XFR61P'
        'ze9lmavj1GD7JIY2wYvx4Eq2URtHo7QarfZI+Z4hbq065DWN6F1c2hqH7TYRPGrP'
        '24TlRIJ97H+vdtNlxS7J4oARKUNZgCZOa1pKq4UznwgfCkyEdHQUzb/VcjEf3MIZ'
        'DIKl8wKBgBrsIjiDvpkfnpmQ7fehEJIi+V4SGskLxFH3ZTvngFFoYry3dL5gQ6mF'
        'sDfrn4igIcEy6bMpJQ3lbwStyzcWZLMJgdI23FTlPXTEG7PclZSuxBpQpvg3MiVO'
        'zqVTrhnY+TemcScSx5O6f32aDfOUWWCzmw/gzvJxUYlJqjqd7dlT'
        '-----END RSA PRIVATE KEY-----'
    ==
  =/  k=key:rsa
    (need (ring:de:pem:pkcs1 kpem))
  ::  generated with openssl, certbot style
  =/  csr-pem=wain
    :~  '-----BEGIN CERTIFICATE REQUEST-----'
        'MIICezCCAWMCAQAwADCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBANoy'
        'afHYACsuXEswxOA/mlGymbEKDISFiNiBZRPgDtBggTIoOascdiCAD1SHEouX4zi7'
        'Ux57iGWMPrNOqnm/ECCkb5GWiMJwNy8+VFK8YIferHs3VwgkhuxZ3SBbJHE/sxtN'
        'CS/nz0XZT47dVYNm95J3qOWgCjCD8jk83GOfdFE3PpbkNx4YEojRd5+WEIduHd5E'
        'tmDlKEl0Nd/mEZY8TXwYF21fgu/4DvJhcaZGn4ZoJCBjXlpak/aACEWvoDsdd7jw'
        'IPiW//JT1fWxmhahr8bwTKd8VAEpc2B4g57XAeTIjj0VugaCBiOagxUmEWQZMvXK'
        'xO1gGIKrT7YNtc78cBcCAwEAAaA2MDQGCSqGSIb3DQEJDjEnMCUwIwYDVR0RBBww'
        'GoINem9kLnVyYml0Lm9yZ4IJem9kLnVyYml0MA0GCSqGSIb3DQEBCwUAA4IBAQCu'
        'HdUqIlW8w7G7l3YxXfb0fn1HxD7zHf3QpNqDYZuDq958OhJNXYJ9EjiHBBEW5ySg'
        'e7vyaaWh6UcVIu/RYFGbyVupNVbp4aS4U1LEEiqMQ6vQQnlSt7hVMi04bhf6X6jd'
        'kkTJIB5OlmNh5z/mjvlIOyOCeitK5IMrStsUPE5F8OynMuzmBKq318LiwImO41Fu'
        'S1M6rgPnvoZeqShLxnJduMcu7awHQ0tn2FmjwBpU713/tmNjtCLYYoBxsHM2ptxd'
        'G3zcMSCDZ/CLXWoktkULdWNDED8meCKWQJxYuHjxY4JPIzIVdVN50xRcZEV7Z80l'
        'bPcFaE8op3e2hIxzqi1t'
        '-----END CERTIFICATE REQUEST-----'
    ==
  =/  hot1  /org/urbit/zod
  =/  hot2  /urbit/zod
  %+  expect-eq
    !>  csr-pem
    !>  (en:pem:pkcs10 k [hot1 hot2 ~])
--

