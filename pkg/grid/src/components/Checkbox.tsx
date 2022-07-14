import React, { useState } from 'react';
import classNames from 'classnames';
import * as RadixCheckbox from '@radix-ui/react-checkbox';
import { CheckIcon } from '@radix-ui/react-icons';

export const Checkbox: React.FC<RadixCheckbox.CheckboxProps> = ({
  defaultChecked,
  checked,
  onCheckedChange,
  disabled,
  className,
  children
}) => {
  const [on, setOn] = useState(defaultChecked);
  const isControlled = !!onCheckedChange;
  const proxyChecked = isControlled ? checked : on;
  const proxyOnCheckedChange = isControlled ? onCheckedChange : setOn;

  return (
    <div className="flex content-center items-center space-x-2">
      <RadixCheckbox.Root
        className={classNames(
          'default-ring border-gray-200 border-2 rounded-sm bg-white h-4 w-4',
          className
        )}
        checked={proxyChecked}
        onCheckedChange={proxyOnCheckedChange}
        disabled={disabled}
        id="checkbox"
      >
        <RadixCheckbox.Indicator className="flex justify-center">
          <CheckIcon className="text-black" />
        </RadixCheckbox.Indicator>
      </RadixCheckbox.Root>
      <label htmlFor="checkbox" className="font-semibold">
        {children}
      </label>
    </div>
  );
};
