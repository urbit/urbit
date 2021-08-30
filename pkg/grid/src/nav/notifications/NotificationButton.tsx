import React from 'react';
import type * as Polymorphic from '@radix-ui/react-polymorphic';
import classNames from 'classnames';

type NotificationButtonVariant = 'primary' | 'secondary' | 'destructive';

type PolymorphicButton = Polymorphic.ForwardRefComponent<
  'button',
  {
    variant?: NotificationButtonVariant;
  }
>;

const variants: Record<NotificationButtonVariant, string> = {
  primary: 'text-blue bg-white',
  secondary: 'text-black bg-white',
  destructive: 'text-red-400 bg-white'
};

export const NotificationButton = React.forwardRef(
  ({ as: Comp = 'button', variant = 'primary', children, className, ...props }, ref) => {
    return (
      <Comp
        ref={ref}
        {...props}
        className={classNames(
          'button p-1 leading-4 font-medium default-ring rounded',
          variants[variant],
          className
        )}
      >
        {children}
      </Comp>
    );
  }
) as PolymorphicButton;
