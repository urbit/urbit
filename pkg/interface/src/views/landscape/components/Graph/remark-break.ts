import blockquote from './blockquote';

function lineBreak(eat, value: string, silent) {
  let index = -1;
  let character: string | null;
  while(++index < length ) {
    character = value.charAt(index);
    if(character === '\n') {
      eat(character)({ type: 'paragraph', contents: [{ type: 'break' }] });
    } else {
      return;
    }
  }
}

lineBreak.locator = function(value, fromIndex) {
  return value.indexOf('\n', fromIndex);
};

export default function plugin() {
  this.Parser.prototype.blockTokenizers.break = lineBreak;
  this.Parser.prototype.inlineTokenizers.break = lineBreak;
  this.Parser.prototype.blockTokenizers.blockquote = blockquote;
}
