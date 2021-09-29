import { cite } from '@urbit/api';
import React, { HTMLAttributes } from 'react';

type ShipNameProps = {
  name: string;
} & HTMLAttributes<HTMLSpanElement>;

export const ShipName = ({ name, ...props }: ShipNameProps) => {
  const separator = /([_^-])/;
  const parts = cite(name).replace('~', '').split(separator);
  const first = parts.shift();

  return (
    <span {...props}>
      <span aria-hidden>~</span>
      <span>{first}</span>
      {parts.length > 1 && (
        <>
          {parts.map((piece) => (
            <span aria-hidden={separator.test(piece)}>{piece}</span>
          ))}
        </>
      )}
    </span>
  );
};
