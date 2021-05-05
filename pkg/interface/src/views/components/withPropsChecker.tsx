import deep_diff from 'deep-diff';
import React, { Component, useEffect, useRef } from 'react';

const withPropsChecker = (WrappedComponent: Component) => {
  return (props: any) => {
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
