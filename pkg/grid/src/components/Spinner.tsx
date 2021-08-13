import classNames from 'classnames';
import React from 'react';
import { SpinnerIcon } from './icons/SpinnerIcon';

export const Spinner = ({ className, ...props }: React.HTMLAttributes<SVGSVGElement>) => (
  <SpinnerIcon className={classNames('spinner', className)} {...props} />
);
