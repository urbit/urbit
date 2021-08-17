import React, { useRef, useEffect} from 'react';

import deep_diff from "deep-diff";

const withPropsChecker = WrappedComponent => {
  return props => {
    const prevProps = useRef(props);
    useEffect(() => {
      const diff = deep_diff.diff(prevProps.current, props);
      if (diff) {
        console.log(diff);
      }
      prevProps.current = props;
    });
    return <WrappedComponent {...props} />;
  };
};

export default withPropsChecker;