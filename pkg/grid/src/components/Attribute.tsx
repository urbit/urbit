import React from 'react';
import cn from 'classnames';
import { capitalize } from 'lodash';

interface AttributeProps {
  attr: string;
  children: React.ReactNode;
  title?: string;
  className?: string;
}

export const Attribute = ({ attr, children, title, className }: AttributeProps) => (
  <div className={cn('h4', className)}>
    <h2 className="mb-2 text-gray-500">{title || capitalize(attr)}</h2>
    <p className="font-mono">{children}</p>
  </div>
);
