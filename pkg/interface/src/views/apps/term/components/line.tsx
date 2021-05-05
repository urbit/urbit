import { Text } from '@tlon/indigo-react';
import React from 'react';

export default React.memo(({ line }) => {
  //  line body to jsx
  // NOTE  lines are lists of characters that might span multiple codepoints
  //
  let text = '';
  if (line.lin) {
    text = line.lin.join('');
  } else if (line.klr) {
    text = line.klr.map((part, i) => {
      const prop = part.stye.deco.reduce((prop, deco) => {
        switch (deco) {
          case null: return prop;
          case 'br': return { bold: true, ...prop };
          case 'bl': return { className: 'blink', ...prop };
          case 'un': return { style: { textDecoration: 'underline' }, ...prop };
          default: console.log('weird deco', deco); return prop;
        }
      }, {});
      switch (part.stye.fore) {
        case null: break;
        case 'r': prop.color = 'red';     break;
        case 'g': prop.color = 'green';   break;
        case 'b': prop.color = 'blue';    break;
        case 'c': prop.color = 'cyan';    break;
        case 'm': prop.color = 'purple';  break;
        case 'y': prop.color = 'yellow';  break;
        case 'k': prop.color = 'black';   break;
        case 'w': prop.color = 'white';   break;
        default: prop.color = '#' + part.stye.fore;
      }
      switch (part.stye.back) {
        case null: break;
        case 'r': prop.backgroundColor = 'red';     break;
        case 'g': prop.backgroundColor = 'green';   break;
        case 'b': prop.backgroundColor = 'blue';    break;
        case 'c': prop.backgroundColor = 'cyan';    break;
        case 'm': prop.backgroundColor = 'purple';  break;
        case 'y': prop.backgroundColor = 'yellow';  break;
        case 'k': prop.backgroundColor = 'black';   break;
        case 'w': prop.backgroundColor = 'white';   break;
        default: prop.backgroundColor = '#' + part.stye.back;
      }
      if (Object.keys(prop).length === 0) {
        return part.text;
      } else {
        return (<Text mono fontSize='inherit' key={i} {...prop}>
          {part.text.join('')}
        </Text>);
      }
    });
  }

  //  render line
  //
  return (
    <Text mono display='flex'
fontSize='0'
    style={{ overflowWrap: 'break-word', whiteSpace: 'pre-wrap' }}
    >
      {text}
    </Text>
  );
});
