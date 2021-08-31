import React from 'react';
import { Docket } from '@urbit/api/docket';
import cn from 'classnames';

interface DocketImageProps extends Pick<Docket, 'color' | 'image'> {
  className?: string;
  sizing?: 'small' | 'full';
}

export function DocketImage({ color, image, className = '', sizing = 'full' }: DocketImageProps) {
  const sizingClass =
    sizing === 'full'
      ? 'w-full h-full md:w-full md:h-full rounded-md'
      : 'w-12 h-12 md:w-20 md:h-20 rounded-xl';

  return (
    <div
      className={cn(sizingClass, `flex-none relative bg-gray-200`, className)}
      style={{ backgroundColor: color }}
    >
      {image && (
        <img
          className="absolute top-1/2 left-1/2 h-[40%] w-[40%] object-contain transform -translate-x-1/2 -translate-y-1/2"
          src={image}
          alt=""
        />
      )}
    </div>
  );
}
