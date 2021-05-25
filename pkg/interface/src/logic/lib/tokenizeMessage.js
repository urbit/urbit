import urbitOb from 'urbit-ob';
import { parsePermalink, permalinkToReference } from '~/logic/lib/permalinks';

const URL_REGEX = new RegExp(String(/^(.*?)(([\w\-\+]+:\/\/)[-a-zA-Z0-9:@;?&=\/%\+\.\*!'\(\),\$_\{\}\^~\[\]`#|]+\w)([\s\S]*)/.source));

const PATP_REGEX = /^(.)(~[a-z_-]+)(.*)/;

const GROUP_REGEX = new RegExp(String(/^( *)(~[-a-z_]+\/[-a-z]+)(.*)/.source));

const convertToGroupRef = group => `web+urbitgraph://group/${group}`;

export const isUrl = (str) => {
  try {
    return URL_REGEX.test(str);
  } catch (e) {
    return false;
  }
};

const tokenizeMessage = (text) => {
  console.log(text);
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
      console.log(str);
      const resetAndPush = (content) => {
        blocks.push(currBlock.join(''));
        messages.push({ text: blocks.join('`') });
        currBlock = [];
        blocks = [];
        messages.push(content);
      };
      const link = str.match(URL_REGEX);
      if(link) {
        const [,pfix, url, protocol, sfix] = link;
        const perma = parsePermalink(url);
        currBlock.push(pfix);
        if(protocol === 'web+urbitgraph://' && perma) {
          resetAndPush(permalinkToReference(perma));
        } else {
          resetAndPush({ url });
        }
        str = sfix;
        continue;
      }
      const groupRef = str.match(GROUP_REGEX);
      if(groupRef) {
        const [,pfix, group, sfix] = groupRef;
        currBlock.push(pfix);
        const perma = parsePermalink(convertToGroupRef(group));
        resetAndPush(permalinkToReference(perma));
        str = sfix;
        continue;
      }
      const patp = str.match(PATP_REGEX);
      if(Boolean(patp) && urbitOb.isValidPatp(patp[2])) {
        const [,pfix, mention, sfix] = patp;
        currBlock.push(pfix);
        resetAndPush({ mention });
        str = sfix;
        continue;
      }
      currBlock.push(str);
      str = '';
    }
    blocks.push(currBlock.join(''));
    currBlock = [];
  });
  messages.push({ text: blocks.join('`') });

  return messages;
};

export { tokenizeMessage as default, URL_REGEX };
