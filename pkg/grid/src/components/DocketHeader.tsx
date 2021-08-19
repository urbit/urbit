import React from 'react';
import { Docket } from '../state/docket-types';

interface DocketHeaderProps {
  docket: Docket;
  children?: React.ReactNode;
}

export function DocketHeader(props: DocketHeaderProps) {
  const { docket, children } = props;
  const { info, title, img, color } = docket;

  return (
    <header className="grid grid-cols-[5rem,1fr] md:grid-cols-[8rem,1fr] auto-rows-min grid-flow-row-dense mb-5 sm:mb-8 gap-x-6 gap-y-4">
      <div
        className="flex-none row-span-1 md:row-span-2 relative w-20 h-20 md:w-32 md:h-32 bg-gray-200 rounded-xl"
        style={{ backgroundColor: color }}
      >
        {img && (
          <img
            className="absolute top-1/2 left-1/2 h-[40%] w-[40%] object-contain transform -translate-x-1/2 -translate-y-1/2"
            src={img}
            alt=""
          />
        )}
      </div>
      <div className="col-start-2">
        <h1 className="h2">{title}</h1>
        {info && <p className="h4 mt-2 text-gray-500">{info}</p>}
      </div>
      {children}
    </header>
  );
}
