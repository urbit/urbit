/-  asn1
/+  primitive-rsa, *pkcs, *jose, *test
=,  eyre
=*  rsa  primitive-rsa
|%
++  test-jwk
  :: rfc7638 section 3.1
  =/  n
    :~  '0vx7agoebGcQSuuPiLJXZptN9nndrQmbXEps2'
        'aiAFbWhM78LhWx4cbbfAAtVT86zwu1RK7aPFFxuhDR1L6tSoc_BJECPebWKRXjBZCi'
        'FV4n3oknjhMstn64tZ_2W-5JsGY4Hc5n9yBXArwl93lqt7_RN5w6Cf0h4QyQ5v-65Y'
        'GjQR0_FDW2QvzqY368QQMicAtaSqzs8KJZgnYb9c7d0zgdAZHzu6qMQvRL5hajrn1n'
        '91CbOpbISD08qNLyrdkt-bFTWhAI4vMQFh6WeZu0fM4lFd2NcRwr3XPksINHaQ-G_x'
        'BniIqbw0Ls1jF44-csFCur-kEgU8awapJzKnqDKgw'
    ==
  =/  jk=json
    :-  %o  %-  my  :~
      kty+s+'RSA'
      n+s+(rap 3 n)
      e+s+'AQAB'
    ==
  =/  k  (need (pass:de:jwk jk))
  ;:  weld
    %+  expect-eq
      !>  jk
      !>  (pass:en:jwk k)
  ::
    %+  expect-eq
      !>  'NzbLsXh8uDCcd-6MNwXF4W_7noWXFZAfHkxZsRGC9Xs'
      !>  (pass:thumb:jwk k)
  ==
::
++  test-jws
  ::  rfc7515 appendix 2
  =/  pt=@t
    %+  rap  3
    :~  '4BzEEOtIpmVdVEZNCqS7baC4crd0pqnRH_5IB3jw3bcxGn6QLvnEtfdUdi'
        'YrqBdss1l58BQ3KhooKeQTa9AB0Hw_Py5PJdTJNPY8cQn7ouZ2KKDcmnPG'
        'BY5t7yLc1QlQ5xHdwW1VhvKn-nXqhJTBgIPgtldC-KDV5z-y2XDwGUc'
    ==
  =/  qt=@t
    %+  rap  3
    :~  'uQPEfgmVtjL0Uyyx88GZFF1fOunH3-7cepKmtH4pxhtCoHqpWmT8YAmZxa'
        'ewHgHAjLYsp1ZSe7zFYHj7C6ul7TjeLQeZD_YwD66t62wDmpe_HlB-TnBA'
        '-njbglfIsRLtXlnDzQkv5dTltRJ11BKBBypeeF6689rjcJIDEz9RWdc'
    ==
  =/  nt=@t
    %+  rap  3
    :~  'ofgWCuLjybRlzo0tZWJjNiuSfb4p4fAkd_wWJcyQoTbji9k0l8W26mPddx'
        'HmfHQp-Vaw-4qPCJrcS2mJPMEzP1Pt0Bm4d4QlL-yRT-SFd2lZS-pCgNMs'
        'D1W_YpRPEwOWvG6b32690r2jZ47soMZo9wGzjb_7OMg0LOL-bSf63kpaSH'
        'SXndS5z5rexMdbBYUsLA9e-KXBdQOS-UTo7WTBEMa2R2CapHg665xsmtdV'
        'MTBQY4uDZlxvb3qCo5ZwKh9kG4LT6_I5IhlJH7aGhyxXFvUK-DWNmoudF8'
        'NAco9_h9iaGNj8q2ethFkMLs91kzk2PAcDTW9gb54h4FRWyuXpoQ'
     ==
  =/  dt=@t
    %+  rap  3
    :~  'Eq5xpGnNCivDflJsRQBXHx1hdR1k6Ulwe2JZD50LpXyWPEAeP88vLNO97I'
        'jlA7_GQ5sLKMgvfTeXZx9SE-7YwVol2NXOoAJe46sui395IW_GO-pWJ1O0'
        'BkTGoVEn2bKVRUCgu-GjBVaYLU6f3l9kJfFNS3E0QbVdxzubSu3Mkqzjkn'
        '439X0M_V51gfpRLI9JYanrC4D4qAdGcopV_0ZHHzQlBjudU2QvXt4ehNYT'
        'CBr6XCLQUShb1juUO1ZdiYoFaFQT5Tw8bGUl_x_jTj3ccPDVZFD9pIuhLh'
        'BOneufuBiB4cS98l2SR_RQyGWSeWjnczT0QU91p1DhOVRuOopznQ'
    ==
  =/  jk=json
    :-  %o  %-  my  :~
      kty+s+'RSA'
      n+s+nt
      e+s+'AQAB'
      d+s+dt
      p+s+pt
      q+s+qt
    ==
  =/  k=key:rsa  (need (ring:de:jwk jk))
  =/  hed=json  o+(my alg+s+'RS256' ~)
  =/  hedt=@t  'eyJhbGciOiJSUzI1NiJ9'
  =/  lod=json
    :-  %o  %-  my  :~
      iss+s+'joe'
      exp+n+'1300819380'
      ['http://example.com/is_root' %b &]
    ==
  =/  lodt=@t
    %+  rap  3
    :~  'eyJpc3MiOiJqb2UiLCJleHAiOjEzMDA4MTkzODAsImh0dHA'
        '6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ'
    ==
  ::  rfc example includes whitespace in json serialization
  =/  lodt-ws=@t
    %+  rap  3
    :~  'eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQo'
        'gImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ'
    ==
  =/  inp-ws=@t
    (rap 3 [hedt '.' lodt-ws ~])
  =/  exp-ws=@t
    %+  rap  3
    :~  'cC4hiUPoj9Eetdgtv3hF80EGrhuB__dzERat0XF9g2VtQgr9PJbu3XOiZj5RZmh7'
        'AAuHIm4Bh-0Qc_lF5YKt_O8W2Fp5jujGbds9uJdbF9CUAr7t1dnZcAcQjbKBYNX4'
        'BAynRFdiuB--f_nZLgrnbyTyWzO75vRK5h6xBArLIARNPvkSjtQBMHlb1L07Qe7K'
        '0GarZRmB_eSN9383LcOLn6_dO--xi12jzDwusC-eOkHWEsqtFZESc6BfI7noOPqv'
        'hJ1phCnvWh6IeYI2w9QOYEUipUTI8np6LbgGY9Fs98rqVt5AXLIhWkWywlVmtVrB'
        'p0igcN_IoypGlUPQGe77Rw'
    ==
  =/  lod-order=(list @t)  ['iss' 'exp' 'http://example.com/is_root' ~]
  ?>  ?=(^ sek.k)
  ;:  weld
    %+  expect-eq
      !>  jk
      !>  (ring:en:jwk k)
  ::
    %+  expect-eq
      !>  n.pub.k
      !>  `@ux`(mul p.u.sek.k q.u.sek.k)
  ::
    %+  expect-eq
      !>  d.u.sek.k
      !>  `@ux`(~(inv fo (elcm:rsa (dec p.u.sek.k) (dec q.u.sek.k))) e.pub.k)
  ::
    %+  expect-eq
      !>  hedt
      !>  (en-base64url (as-octt:mimes:html (en-json-sort aor hed)))
  ::
    %+  expect-eq
      !>  lodt
      !>  %-  en-base64url
          (as-octt:mimes:html (en-json-sort (eor lte lod-order) lod))
  ::
    %+  expect-eq
      !>  exp-ws
      !>  (en-base64url (en:octn (~(sign rs256 k) (met 3 inp-ws) inp-ws)))
  ==
::
++  test-jws-2
  :: captured from an in-the-wild failure
  :: relevant sha-256 has a significant leading zero
  :: which was not being captured in the asn.1 digest ...
  =/  kpem=wain
    :~  '-----BEGIN RSA PRIVATE KEY-----'
        'MIIEogIBAAKCAQEAkmWLu+9gyzCbrGAHTFE6Hs7CtVQofONmpnhmE7JQkmdS+aph'
        'WwZQfp9p6RU6vSoBaPXD96uqMXhvoOXz9/Ub5TRwLmQzfHZdksfU3pEZ8qFMikZU'
        'p5v+CyBnLq9YR0VXN+/JVatmYb1hhC1k101X9m+IU3DR3U+kyCZnXuOd10xVX05H'
        '0pXl+nI25bZyMJFnz1Xfw1rTnhtU/w7bgCWYdMii5jLkl5zfoY2gulpPu7QeYa4K'
        '3fTqklDNFK7kQQ1l4O3461fbSO0cnG4t8Vk3026ageA54+Qx8O8UDi8k18Z1NF+B'
        'pbPUZn55/InuZ8iGyHBZ4GRFIPG0iOdWM7gHCwIDAQABAoIBAAMQN/9SS6MJMULq'
        'CsXHxyl5sHtXa/BgWLHP+j2/FtRX++EkR0s+ln2FobZa+l5Q9m4Ljn5PbqSMAFfM'
        'Y6u0hNyj9om04oOl8bILl4Vcvqgp51oFvAEGOW15/o69+6bS3aBx7cqwfnsivInr'
        'nIXDvHcyey3kh9WCKNx3rxNVgfuTCkw0+K2qXkMTh2c3Iz2efR2f78qbNWQcBe1+'
        's83fABafxACYuXzfOYoO01GBCJnHrmXxJVePLXwxLkLeJHOQJQgPnagVbUH4kbUp'
        'OLd9h1dOVYKpyVaxbQiAH3U/ekOXCCv18a47/PQSbueolzSzMzwVPSZdf+88lzuq'
        'ZZyDXDECgYEAk5zt4cO7X+8IIeNXx8/2pztT9WmC1kqw4RtInoVXm62K1B0pPndW'
        'm0nMVFEDuSwdn61G5amlaOT0dTFHlMFydC9H+1L5PMK7d+6ArSeAtMWoUhz+jkcO'
        'B9KoMfZ9CtP2r5589zDGir8kaY8Fia5Z7TohpJDidmuumgDabl+qH+kCgYEA/eP6'
        'lIGVHF8EIrfewjLM+8i1RE/hzItOpegrwDUVeYfZlPM59xUyC9REdgvmnTssxPcL'
        '2+EB11wvcImSPLuwN0kXUkh9qZUkr9hvYlikALNH1f8WhCJ0kT6pUeA7LbjU4/bM'
        'fsgcOh1POW2piIMERl1TuNRZg7JdKuCJKax3qtMCgYB2dxcifOc/0qIAMGgeX/Rf'
        'ueljp03tlPvnbPIW5oSs19X27YBQNY44Cj4F3Q7T6WfM4k9nuYKacEUQWIBODgJA'
        '5EEsniaQcOfrFGoIjQ9qBMdVPxe8L6I+/P0nO96Wdg4gW12HNIniiAw8+x9Co75f'
        '+KtPW0ekKj9yMQUcV4I9IQKBgE06bruDmzbRFDH3WjQaPc4M5E6OOfH9IgRHVh+W'
        'Rhz8nMu5HJWzBdEhVV3PCuwi1uBnAV112RiIOwnxXuFIejam7ggics8Fxe4TWPZC'
        'Xki0QBKxEElLLcgMlnaITZf/1AovxU5/Uk6/IZ0nZV1X9RHuS4w6U6xCsiJbwH1D'
        'r/bvAoGAV/Vx+Z2BD7QhmHofu98OMW6EGSjWMgOI4iXdcQ80Urz9akHkOM4KGojq'
        'UDobbxxkJt1K5Dzux+vnp1siiIkcLdVdtMzqo7KcKYWonMqZmppNqIFCXQHscCRD'
        'r6f1TIjlurYrazLAkRsmjE5uYM13/E1UdxplWSkdCbivIWqoqTM='
        '-----END RSA PRIVATE KEY-----'
    ==
  =/  k=key:rsa
    (need (ring:de:pem:pkcs1 kpem))
  =/  kid=@t
    'https://acme-staging-v02.api.letsencrypt.org/acme/acct/6336694'
  =/  non=@t
    'a5Pwh6GcuqRSvHTQouW96XNg3iiMORMkBf_wSLOf0M4'
  =/  url=purl
    :-  [sec=%.y por=~ hot=[%.y p=/org/letsencrypt/api/acme-staging-v02]]
    :_  query=~
    :-  ext=~
    %+  weld
      /acme/challenge
    /'efJn0ywfjIi3M7yT-6H8Mdq85R2LnI8XsTG3DaaY8Gc'/'138087558'
  =/  protected-header=json
    :-  %o  %-  my  :~
      nonce+s+non
      url+s+(crip (en-purl:html url))
      kid+s+kid
    ==
  =/  bod=json
    [%o ~]
  =/  exp=json
    =/  payload=@t  'e30'
    =/  protected=@t
      %+  rap  3
      :~  'eyJhbGci'
          'OiJSUzI1NiIsImtpZCI6Imh0dHBzOi8vYWNtZS1zdGFnaW5nLXYwMi5hcGkubGV0c2'
          'VuY3J5cHQub3JnL2FjbWUvYWNjdC82MzM2Njk0Iiwibm9uY2UiOiJhNVB3aDZHY3Vx'
          'UlN2SFRRb3VXOTZYTmczaWlNT1JNa0JmX3dTTE9mME00IiwidXJsIjoiaHR0cHM6Ly'
          '9hY21lLXN0YWdpbmctdjAyLmFwaS5sZXRzZW5jcnlwdC5vcmcvYWNtZS9jaGFsbGVu'
          'Z2UvZWZKbjB5d2ZqSWkzTTd5VC02SDhNZHE4NVIyTG5JOFhzVEczRGFhWThHYy8xMz'
          'gwODc1NTgifQ'
      ==
    =/  signature=@t
      %+  rap  3
      :~  'cukOS_KIWTolvORyJoIu5eejdLoFi6xpd06Y6nW565zFMKZi44BepsWIZXw4yxYjxs'
          '8xFdoKOxtXhBS5BT0mbkHSUGokAPTUiF5b1wjm00ZiKRYwnIotizsLPzHAJKwhMlFs'
          'x6oAu25mmremBgnNtVD_cskQBbkTBgiTL6alrkrmwxlP2gSqyX6uEO-UCY71QB_xYj'
          '4IOoX2k0jdXJevXDAJSUWfs5cZkm8Ug_q4GVTRWhZmFHMnMzonmCC4Ui7nDa9oKJH5'
          'Npyn74FCcqbz111AK-Aul1dNhz3ojE1VOk3eVjH69lSGsaMleYR5fi60Jdc5ZbpPPy'
          't-CZRp1F0k6w'
      ==
    [%o (my payload+s+payload protected+s+protected signature+s+signature ~)]
  %+  expect-eq
    !>  exp
    !>  (sign:jws k protected-header bod)
--
