import _ from 'lodash';
import classnames from 'classnames';

export function makeRoutePath(resource, includeQuery = false) {
  let query = window.location.href.split('?')[1];
  if (includeQuery && query) {
    query = '?' + query;
  } else {
    query = '';
  }
  return '/~debug/' + resource + query;
}

export function msToDa(ms, mil) {
  const d = new Date(ms);
  var fil = function(n) {
    return n >= 10 ? n : "0" + n;
  };
  return (
    `~${d.getUTCFullYear()}.` +
    `${(d.getUTCMonth() + 1)}.` +
    `${fil(d.getUTCDate())}..` +
    `${fil(d.getUTCHours())}.` +
    `${fil(d.getUTCMinutes())}.` +
    `${fil(d.getUTCSeconds())}` +
    `${mil ? "..0000" : ""}`
  );
}

export function renderDuct(duct) {
  return duct.reduce((a, b) => a + b + ' ', '');
}
