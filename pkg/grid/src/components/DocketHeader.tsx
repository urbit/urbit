import React from 'react';
import { DocketImage } from './DocketImage';
import { getAppName } from '../state/util';
import { DocketWithDesk } from '../state/docket';

interface DocketHeaderProps {
  docket: DocketWithDesk;
  children?: React.ReactNode;
}

export function DocketHeader(props: DocketHeaderProps) {
  const { docket, children } = props;
  const { info, image, color } = docket;

  return (
    <header className="grid grid-cols-[5rem,1fr] md:grid-cols-[8rem,1fr] auto-rows-min grid-flow-row-dense mb-5 sm:mb-8 gap-x-6 gap-y-4">
      <DocketImage color={color} image={image} className="row-span-1 md:row-span-2" />
      <div className="col-start-2">
        <h1 className="h2">{getAppName(docket)}</h1>
        {info && <p className="h4 mt-2 text-gray-500">{info}</p>}
      </div>
      {children}
    </header>
  );
}
