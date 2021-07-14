export function uuid() {
  let str = '0v';
  str += Math.ceil(Math.random() * 8) + '.';
  for (var i = 0; i < 5; i++) {
    let _str = Math.ceil(Math.random() * 10000000).toString(32);
    _str = ('00000' + _str).substr(-5, 5);
    str += _str + '.';
  }

  return str.slice(0, -1);
}

export function isPatTa(str) {
  const r = /^[a-z,0-9,\-,.,_,~]+$/.exec(str);
  return !!r;
}

/*
  Goes from:
    ~2018.7.17..23.15.09..5be5    // urbit @da
  To:
    (javascript Date object)
*/
export function daToDate(st) {
  var dub = function (n) {
    return parseInt(n) < 10 ? '0' + parseInt(n) : n.toString();
  };
  var da = st.split('..');
  var bigEnd = da[0].split('.');
  var lilEnd = da[1].split('.');
  var ds = `${bigEnd[0].slice(1)}-${dub(bigEnd[1])}-${dub(bigEnd[2])}T${dub(
    lilEnd[0]
  )}:${dub(lilEnd[1])}:${dub(lilEnd[2])}Z`;
  return new Date(ds);
}

/*
  Goes from:
    (javascript Date object)
  To:
    ~2018.7.17..23.15.09..5be5    // urbit @da
*/

export function dateToDa(d, mil) {
  var fil = function (n) {
    return n >= 10 ? n : '0' + n;
  };
  return (
    `~${d.getUTCFullYear()}.` +
    `${d.getUTCMonth() + 1}.` +
    `${fil(d.getUTCDate())}..` +
    `${fil(d.getUTCHours())}.` +
    `${fil(d.getUTCMinutes())}.` +
    `${fil(d.getUTCSeconds())}` +
    `${mil ? '..0000' : ''}`
  );
}

export function deSig(ship) {
  return ship.replace('~', '');
}

// trim patps to match dojo, chat-cli
export function cite(ship) {
  let patp = ship,
    shortened = '';
  if (patp.startsWith('~')) {
    patp = patp.substr(1);
  }
  // comet
  if (patp.length === 56) {
    shortened = '~' + patp.slice(0, 6) + '_' + patp.slice(50, 56);
    return shortened;
  }
  // moon
  if (patp.length === 27) {
    shortened = '~' + patp.slice(14, 20) + '^' + patp.slice(21, 27);
    return shortened;
  }
  return `~${patp}`;
}

export function satsToCurrency(sats, denomination, rates) {
  if (!rates) {
    throw 'nonexistent currency table';
  }
  if (!rates[denomination]) {
    denomination = 'BTC';
  }
  let rate = rates[denomination];
  let val = rate ? parseFloat((sats * rate.last * 0.00000001).toFixed(8)) : 0;
  let text;
  if (denomination === 'BTC' && rate) {
    text = val + ' ' + rate.symbol;
  } else if (rate) {
    text =
      rate.symbol + val.toFixed(2).replace(/(\d)(?=(\d{3})+(?!\d))/g, '$1,');
  }
  return text;
}

export function currencyToSats(val, denomination, rates) {
  if (!rates) {
    throw 'nonexistent currency table';
  }
  if (!rates[denomination]) {
    throw 'currency not in table';
  }
  let rate = rates[denomination];
  let sats = (parseFloat(val) / rate.last) * 100000000;
  return sats;
}

export function reduceHistory(history) {
  return Object.values(history).sort((hest1, hest2) => {
    if (hest1.recvd === null) return -1;
    if (hest2.recvd === null) return +1;
    return hest2.recvd - hest1.recvd;
  });
}

export function mapDenominationToSymbol(denomination) {
  switch (denomination) {
    case 'USD':
      return '$';
    default:
      return denomination;
  }
}
