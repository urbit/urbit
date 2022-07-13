import classNames from 'classnames';
import React, { FC, HTMLAttributes } from 'react';
import slugify from 'slugify';
import { useAsyncCall } from '../logic/useAsyncCall';
import { Spinner } from './Spinner';
import { Toggle } from './Toggle';

type SettingsProps = {
  name: string;
  on: boolean;
  disabled?: boolean;
  toggle: (open: boolean) => Promise<void>;
} & HTMLAttributes<HTMLDivElement>;

export const Setting: FC<SettingsProps> = ({
  name,
  on,
  disabled = false,
  toggle,
  className,
  children
}) => {
  const { status, call } = useAsyncCall(toggle);
  const id = slugify(name);

  return (
    <section className={className}>
      <div className="flex space-x-2">
        <Toggle
          aria-labelledby={id}
          pressed={on}
          onPressedChange={call}
          className="flex-none self-start text-blue-400"
          disabled={disabled}
          loading={status === 'loading'}
        />
        <div className="flex-1 flex flex-col justify-center">
          <h3 id={id} className="flex items-center font-semibold leading-6">
            {name} {status === 'loading' && <Spinner className="h-4 w-4 ml-2" />}
          </h3>
          {children}
        </div>
      </div>
    </section>
  );
};
