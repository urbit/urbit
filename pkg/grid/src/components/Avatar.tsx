import classNames from 'classnames';
import React, { useMemo } from 'react';
import { sigil, reactRenderer } from '@tlon/sigil-js';
import { deSig, Contact } from '@urbit/api';

export type AvatarSizes = 'xs' | 'small' | 'default';

interface AvatarProps extends Contact {
  shipName: string;
  size: AvatarSizes;
  className?: string;
}

interface AvatarMeta {
  classes: string;
  size: number;
}

const sizeMap: Record<AvatarSizes, AvatarMeta> = {
  xs: { classes: 'w-6 h-6 rounded', size: 12 },
  small: { classes: 'w-8 h-8 rounded-lg', size: 16 },
  default: { classes: 'w-12 h-12 rounded-lg', size: 24 }
};

const foregroundFromBackground = (background: string): 'black' | 'white' => {
  const rgb = {
    r: parseInt(background.slice(1, 3), 16),
    g: parseInt(background.slice(3, 5), 16),
    b: parseInt(background.slice(5, 7), 16)
  };
  const brightness = (299 * rgb.r + 587 * rgb.g + 114 * rgb.b) / 1000;
  const whiteBrightness = 255;

  return whiteBrightness - brightness < 50 ? 'black' : 'white';
};

const emptyContact: Contact = {
  nickname: '',
  bio: '',
  status: '',
  color: '#000000',
  avatar: null,
  cover: null,
  groups: [],
  'last-updated': 0
};

export const Avatar = ({ size, className, ...ship }: AvatarProps) => {
  const { shipName, color, avatar } = { ...emptyContact, ...ship };
  const { classes, size: sigilSize } = sizeMap[size];
  const foregroundColor = foregroundFromBackground(color);
  const sigilElement = useMemo(() => {
    if (shipName.match(/[_^]/)) {
      return null;
    }

    return sigil({
      patp: deSig(shipName) || 'zod',
      renderer: reactRenderer,
      size: sigilSize,
      icon: true,
      colors: [color, foregroundColor]
    });
  }, [shipName, color, foregroundColor]);

  if (avatar) {
    return <img className={classNames('', classes)} src={avatar} alt="" />;
  }

  return (
    <div
      className={classNames(
        'flex-none relative bg-black rounded-lg',
        classes,
        size === 'xs' && 'p-1.5',
        size === 'small' && 'p-2',
        size === 'default' && 'p-3',
        className
      )}
      style={{ backgroundColor: color }}
    >
      {sigilElement}
    </div>
  );
};
