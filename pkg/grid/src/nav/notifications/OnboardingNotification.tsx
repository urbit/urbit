import React from 'react';
import cn from 'classnames';
import { Button } from '../../components/Button';

const cards: OnboardingCardProps[] = [
  {
    title: 'Terminal',
    body: "Install a web terminal to access your Urbit's command line",
    button: 'Install',
    color: '#9CA4B1'
  },
  {
    title: 'Groups',
    body: 'Install Groups, a suite of social software to communicate with other urbit users',
    button: 'Install',
    color: '#D1DDD3'
  },
  {
    title: 'Bitcoin',
    body: 'Install a bitcoin wallet. You can send bitcoin to any btc address, or even another ship',
    button: 'Install',
    color: '#F6EBDB'
  },
  {
    title: 'Debug',
    body: "Install a debugger. You can inspect your ship's internals using this interface",
    button: 'Install'
  },
  {
    title: 'Build an app',
    body: 'You can instantly get started building new things on Urbit.  Just right click your Landscape and select “New App”',
    button: 'Learn more',
    color: '#82A6CA'
  }
];

interface OnboardingCardProps {
  title: string;
  button: string;
  body: string;
  color?: string;
}

const OnboardingCard = ({ title, button, body, color }: OnboardingCardProps) => (
  <div
    className="p-4 flex flex-col space-y-2 text-black bg-gray-100 justify-between rounded-xl"
    style={color ? { backgroundColor: color } : {}}
  >
    <div className="space-y-1">
      <h4 className="font-semibold text-black">{title}</h4>
      <p>{body}</p>
    </div>
    <Button variant="primary" className="bg-gray-500">
      {button}
    </Button>
  </div>
);

interface OnboardingNotificationProps {
  unread: boolean;
}

export const OnboardingNotification = ({ unread }: OnboardingNotificationProps) => (
  <section
    className={cn('notification space-y-2 text-black', unread ? 'bg-blue-100' : 'bg-gray-100')}
    aria-labelledby=""
  >
    <header id="system-updates-blocked" className="relative space-y-2">
      <div className="flex space-x-2">
        <span className="inline-block w-6 h-6 bg-orange-500 rounded-full" />
        <span className="font-medium">Grid</span>
      </div>
      <div className="flex space-x-2">
        <h2 id="runtime-lag">Hello there, welcome to Grid!</h2>
      </div>
    </header>
    <div className="grid grid-cols-3 grid-rows-2 gap-4">
      {
        /* eslint-disable-next-line react/no-array-index-key */
        cards.map((card, i) => (
          <OnboardingCard key={i} {...card} />
        ))
      }
    </div>
  </section>
);
