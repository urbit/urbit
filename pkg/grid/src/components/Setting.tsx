import classNames from 'classnames';
import React, { FC, HTMLAttributes } from 'react';
import slugify from 'slugify';
import { useAsyncCall } from '../logic/useAsyncCall';
import { Spinner } from './Spinner';
import { Toggle } from './Toggle';

type SettingsProps = {
  name: string;
  on: boolean;
  toggle: (open: boolean) => Promise<void>;
} & HTMLAttributes<HTMLDivElement>;

export const Setting: FC<SettingsProps> = ({ name, on, toggle, className, children }) => {
  const { status, call } = useAsyncCall(toggle);
  const id = slugify(name);

  return (
    <section className={classNames('inner-section', className)}>
      <h3 id={id} className="flex items-center h4 mb-2">
        {name} {status === 'loading' && <Spinner className="ml-2" />}
      </h3>
      <div className="flex space-x-2">
        <Toggle
          aria-labelledby={id}
          pressed={on}
          onPressedChange={call}
          className="flex-none self-start text-blue-400"
        />
        <div className="flex-1 space-y-6">{children}</div>
      </div>
    </section>
  );
};
