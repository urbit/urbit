import React, { useState } from 'react';
import classNames from 'classnames';
import * as RadioGroup from '@radix-ui/react-radio-group';

type prefType = 'system' | 'dark' | 'light';

interface RadioOptionProps {
  value: string;
  label: string;
  selected: boolean;
}

const apperanceOptions = [
  { value: 'system', label: 'System Theme' },
  { value: 'light', label: 'Light' },
  { value: 'dark', label: 'Dark' }
];

const RadioOption = ({ value, label, selected }: RadioOptionProps) => (
  <div className="flex space-x-3 ">
    <RadioGroup.Item
      className={classNames('flex items-center border-gray-200 w-4 h-4 rounded-full', {
        'border-2': !selected
      })}
      value={value}
      id={value}
    >
      <RadioGroup.Indicator className="flex items-center border-4 rounded-full border-gray-800 w-full h-full" />
    </RadioGroup.Item>
    <label className="font-semibold" htmlFor={value}>
      {label}
    </label>
  </div>
);

export const AppearancePrefs = () => {
  const [pref, setPref] = useState<prefType>('system');

  const handleChange = (value: string) => {
    setPref(value as prefType);
  };

  return (
    <div className="inner-section space-y-8">
      <h2 className="h4">Landscape Apperance</h2>
      <RadioGroup.Root
        className="flex flex-col space-y-3"
        value={pref}
        onValueChange={handleChange}
      >
        {apperanceOptions.map((option) => (
          <RadioOption value={option.value} label={option.label} selected={pref === option.value} />
        ))}
      </RadioGroup.Root>
    </div>
  );
};
