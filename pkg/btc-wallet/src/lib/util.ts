import { CurrencyRate, Denomination, Transaction } from '../types';

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

export function isPatTa(str: string) {
  const r = /^[a-z,0-9,\-,.,_,~]+$/.exec(str);
  return !!r;
}

/*
  Goes from:
    ~2018.7.17..23.15.09..5be5    // urbit @da
  To:
    (javascript Date object)
*/
export function daToDate(st: string) {
  var dub = function (n: string) {
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

export function dateToDa(d: Date, mil: boolean) {
  var fil = function (n: number) {
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

export function deSig(ship: string) {
  return ship.replace('~', '');
}

// trim patps to match dojo, chat-cli
export function cite(ship: string) {
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

export function satsToCurrency(
  sats: number,
  denomination: Denomination,
  rates: CurrencyRate
) {
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

export function mapDenominationToSymbol(denomination: string) {
  switch (denomination) {
    case 'USD':
      return '$';
    default:
      return denomination;
  }
}

export function reduceHistory(history: Transaction[]) {
  return Object.values(history).sort((hest1, hest2) => {
    if (hest1.recvd === null) return -1;
    if (hest2.recvd === null) return +1;
    return hest2.recvd - hest1.recvd;
  });
}

export function copyToClipboard(textToCopy: string) {
  // navigator clipboard api needs a secure context (https or localhost)
  if (navigator.clipboard && window.isSecureContext) {
    return navigator.clipboard.writeText(textToCopy);
  } else {
    let textArea = document.createElement('textarea');
    textArea.value = textToCopy;
    textArea.style.position = 'fixed';
    textArea.style.left = '-999999px';
    textArea.style.top = '-999999px';
    document.body.appendChild(textArea);
    textArea.focus();
    textArea.select();
    return new Promise<void>((res, rej) => {
      document.execCommand('copy') ? res() : rej();
      textArea.remove();
    });
  }
}
