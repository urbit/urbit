import { memo } from 'react';

const withMemo = (Component, checkedProps) => {
  function areEqual(prevProps, nextProps) {
    let isEqual = true;
    for (let i = 0; i < checkedProps.length; i++) {
      const checkedProp = checkedProps[i];
      console.log(checkedProp);
      if (JSON.stringify(prevProps[checkedProp]) !== JSON.stringify(nextProps[checkedProp])) {
        isEqual = false;
        break;
      }
    }
    return isEqual;
  }
  
  return memo(Component, areEqual);
};

export default withMemo;
