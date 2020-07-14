import React from 'react';
export function MoreInfo(props) {
  if (!props.isOpen || !props.text) {
    return null;
  }
  return (<div className="moon-gray w-90">
    {props.text}
  </div>);
}
