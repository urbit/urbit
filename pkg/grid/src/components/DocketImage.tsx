import React from 'react';
import { Docket } from '@urbit/api/docket';
import cn from 'classnames';
import { useTileColor } from '../tiles/useTileColor';

type DocketImageSizes = 'xs' | 'small' | 'default' | 'full';

interface DocketImageProps extends Pick<Docket, 'color' | 'image'> {
  className?: string;
  size?: DocketImageSizes;
}

const sizeMap: Record<DocketImageSizes, string> = {
  xs: 'w-6 h-6 mr-2 rounded',
  small: 'w-8 h-8 mr-3 rounded-md',
  default: 'w-12 h-12 mr-3 rounded-lg',
  full: 'w-full h-full rounded-2xl'
};

export function DocketImage({ color, image, className = '', size = 'full' }: DocketImageProps) {
  const { tileColor } = useTileColor(color);
  return (
    <div
      className={cn('flex-none relative bg-gray-200 overflow-hidden', sizeMap[size], className)}
      style={{ backgroundColor: tileColor }}
    >
      {image && (
        <img className="absolute top-0 left-0 h-full w-full object-contain" src={image} alt="" />
      )}
    </div>
  );
}
