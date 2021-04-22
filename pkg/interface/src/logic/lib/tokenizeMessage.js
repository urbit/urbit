import urbitOb from 'urbit-ob';
import { parsePermalink, permalinkToReference } from "~/logic/lib/permalinks";

const URL_REGEX = new RegExp(String(/^(([\w\-\+]+:\/\/)[-a-zA-Z0-9:@;?&=\/%\+\.\*!'\(\),\$_\{\}\^~\[\]`#|]+\w)/.source));

const isUrl = (string) => {
  try {
    return URL_REGEX.test(string);
  } catch (e) {
    return false;
  }
}

const isRef = (str) => {
  return isUrl(str) && str.startsWith("web+urbitgraph://");
}

const tokenizeMessage = (text) => {
  let messages = [];
  // by line
  let currTextBlock = [];
  let isInCodeBlock = false;
  let endOfCodeBlock = false;
  text.split(/\r?\n/).forEach((line, index) => {
    // by space
    let currTextLine = [];
    // A line of backticks enters and exits a codeblock
    if (line.trim().startsWith('```')) {
      // But we need to check if we've ended a codeblock
      endOfCodeBlock = isInCodeBlock;
      isInCodeBlock = (!isInCodeBlock);
    } else {
      endOfCodeBlock = false;
    }

    if (isInCodeBlock || endOfCodeBlock) {
      currTextLine = [line];
    } else {
      const words = line.split(/\s/);
      words.forEach((str, idx) => {
        const last = words.length - 1 === idx;
        if (
          (str.startsWith('`') && str !== '`')
          || (str === '`' && !isInCodeBlock)
        ) {
          isInCodeBlock = true;
        } else if (
          (str.endsWith('`') && str !== '`')
          || (str === '`' && isInCodeBlock)
        ) {
          isInCodeBlock = false;
        }

        if(isRef(str) && !isInCodeBlock) {
          if (currTextLine.length > 0 || currTextBlock.length > 0) {
            // If we're in the middle of a message, add it to the stack and reset
            currTextLine.push('');
            messages.push({ text: currTextBlock.join('\n') + currTextLine.join(' ') });
            currTextBlock = last ? [''] : [];
            currTextLine = [];
          }
          const link = parsePermalink(str);
          if(!link) {
            messages.push({ url: str });
          } else {
            const reference = permalinkToReference(link);
            messages.push(reference);
          }
          currTextLine = [];
        } else if (isUrl(str) && !isInCodeBlock) {
          if (currTextLine.length > 0 || currTextBlock.length > 0) {
            // If we're in the middle of a message, add it to the stack and reset
            currTextLine.push('');
            messages.push({ text: currTextBlock.join('\n') + currTextLine.join(' ') });
            currTextBlock = last ? [''] : [];
            currTextLine = [];
          }
          messages.push({ url: str });
          currTextLine = [];
        } else if(urbitOb.isValidPatp(str) && !isInCodeBlock) {
          if (currTextLine.length > 0 || currTextBlock.length > 0) {
            // If we're in the middle of a message, add it to the stack and reset
            currTextLine.push('');
            messages.push({ text: currTextBlock.join('\n') + currTextLine.join(' ') });
            currTextBlock = last ? [''] : [];
            currTextLine = [];
          }
          messages.push({ mention: str });
          currTextLine = [];

        } else {
          currTextLine.push(str);
        }
      });
    }
    currTextBlock.push(currTextLine.join(' '))
  });

  if (currTextBlock.length) {
    // Add any remaining message
    messages.push({ text: currTextBlock.join('\n') });
  }
  return messages;
};

export { tokenizeMessage as default, isUrl, URL_REGEX };
