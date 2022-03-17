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
    <div className="flex content-center space-x-2">
      <RadixCheckbox.Root
        className={classNames('default-ring rounded-lg bg-white h-7 w-7', className)}
        checked={proxyChecked}
        onCheckedChange={proxyOnCheckedChange}
        disabled={disabled}
        id="checkbox"
      >
        <RadixCheckbox.Indicator className="flex justify-center">
          <CheckIcon className="text-black" />
        </RadixCheckbox.Indicator>
      </RadixCheckbox.Root>
      <label htmlFor="checkbox">{children}</label>
    </div>
  );
};
