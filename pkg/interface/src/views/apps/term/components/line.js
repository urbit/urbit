import React, { Component, useMemo } from 'react';
import { Box, Text } from '@tlon/indigo-react';

export default React.memo(({line}) => {

  //  line body to jsx
  //
  let text = '';
  if (line.lin) {
    text = line.lin;
  }
  else if (line.klr) {
    text = line.klr.map((part, i) => {
      let prop = part.stye.deco.reduce((prop, deco) => {
        switch (deco) {
          case null: return prop;
          case 'br': return {bold: true, ...prop};
          case 'bl': return {blink: true, ...prop};  //TODO
          case 'un': return {textDecoration: 'underline', ...prop};  //TODO  fixme
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
        default: console.log('weird fore', part.stye.fore);
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
        default: console.log('weird back', part.stye.back);
      }
      if (Object.keys(prop).length === 0)
      {
        return part.text;
      } else {
        return (<Text mono fontSize='inherit' key={i} {...prop}>{part.text}</Text>);
      }
    });
  }

  //  render line
  //
  return (
    <Text mono display='block' fontSize='14px'
    style={{ overflowWrap: 'break-word', whiteSpace: 'pre-wrap' }}
    >
      {text}
    </Text>
  );

});
