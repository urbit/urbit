import urbitOb from 'urbit-ob';
import { parsePermalink, permalinkToReference } from '~/logic/lib/permalinks';

const URL_REGEX = new RegExp(String(/^([^[\]]*?)(([\w\-\+]+:\/\/)[-a-zA-Z0-9:@;?&=\/%\+\.\*!'\(\),\$_\{\}\^~\[\]`#|]+[-a-zA-Z0-9:@;?&=\/%\+\*!'\(\)\$_\{\}\^~\[\]`#|])([\s\S]*)/.source));

const PATP_REGEX = /^([\s\S]*?)(~[a-z_-]+)([\s\S]*)/;

const GROUP_REGEX = new RegExp(String(/^([\s\S ]*?)(~[-a-z_]+\/[-a-z0-9]+)([\s\S]*)/.source));

const convertToGroupRef = group => `web+urbitgraph://group/${group}`;

export const isUrl = (str) => {
  try {
    return URL_REGEX.test(str);
  } catch (e) {
    return false;
  }
};

const raceRegexes = (str) => {
  const link = str.match(URL_REGEX);
  const groupRef = str.match(GROUP_REGEX);
  const mention = str.match(PATP_REGEX);
  let pfix = str;
  let content, sfix;
  if(link) {
    pfix = link[1];
    sfix = link[4];
    const perma = parsePermalink(link[2]);
    if(perma) {
      content = permalinkToReference(perma);
    } else {
      content = { url: link[2] };
    }
  }
  const perma = parsePermalink(convertToGroupRef(groupRef?.[2]));
  const [,,host] = perma?.group.split('/') ?? [];
  if(groupRef && groupRef[1].length < pfix?.length && Boolean(perma) && urbitOb.isValidPatp(host)) {
    pfix = groupRef[1];
    content = permalinkToReference(perma);
    sfix = groupRef[3];
  }
  if(mention && urbitOb.isValidPatp(mention[2]) && mention[1].length < pfix?.length) {
    pfix = mention[1];
    content = { mention: mention[2] };
    sfix = mention[3];
  }
  return [pfix, content, sfix];
};

const tokenizeMessage = (text) => {
  const messages = [];
  // by line
  let blocks = [];
  let currBlock = [];
  const foo = text.split('`');
  foo.forEach((str, index) => {
    const isCode = index % 2 === 1;
    if(isCode) {
      blocks.push(str);
      return;
    }
    if(str.length === 0) {
      blocks.push('');
      return;
    }
    while(str.length > 0) {
      const resetAndPush = (content) => {
        if(currBlock.length > 0) {
          blocks.push(currBlock.join(''));
        }
        if(blocks.length > 0) {
          //  ended on a `
          if(blocks.length % 2 === 0) {
            blocks.push('');
          }
          messages.push({ text: blocks.join('`') });
        }
        currBlock = [];
        blocks = [];
        messages.push(content);
      };
      const [pfix, content, sfix] = raceRegexes(str);
      if(content) {
        pfix?.length > 0 && currBlock.push(pfix);
        resetAndPush(content);
        str = sfix;
      } else {
        currBlock.push(str);
        str = '';
      }
    }
    blocks.push(currBlock.join(''));
    currBlock = [];
  });
  // ended on a `
  if(blocks.length % 2 === 0) {
    blocks.push('');
  }
  messages.push({ text: blocks.join('`') });
  return messages;
};

export { tokenizeMessage as default, URL_REGEX };
