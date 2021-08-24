import React from 'react';
import { Docket } from '@urbit/api/docket';
import cn from 'classnames';

interface DocketImageProps extends Pick<Docket, 'color' | 'image'> {
  small?: boolean;
  className?: string;
}

export function DocketImage({ color, image, className = '', small = false }: DocketImageProps) {
  const sizing = small
    ? 'w-4 h-4 md:w-6 md:h-6 rounded-md'
    : 'w-12 h-12 md:w-20 md:h-20 rounded-xl';

  return (
    <div
      className={cn(sizing, `flex-none relative bg-gray-200`, className)}
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
