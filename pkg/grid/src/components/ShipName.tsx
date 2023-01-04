import { cite } from '@urbit/api';
import React, { HTMLAttributes } from 'react';

type ShipNameProps = {
  name: string;
  truncate?: boolean;
} & HTMLAttributes<HTMLSpanElement>;

export const ShipName = ({ name, truncate = true, ...props }: ShipNameProps) => {
  const separator = /([_^-])/;
  const citedName = truncate ? cite(name) : name;

  if (!citedName) {
    return null;
  }

  const parts = citedName.replace('~', '').split(separator);
  const first = parts.shift();

  return (
    <span {...props}>
      <span aria-hidden>~</span>
      <span>{first}</span>
      {parts.length > 1 && (
        <>
          {parts.map((piece, index) => (
            <span key={`${piece}-${index}`} aria-hidden={separator.test(piece)}>
              {piece}
            </span>
          ))}
        </>
      )}
    </span>
  );
};
