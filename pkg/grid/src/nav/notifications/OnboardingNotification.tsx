import React from 'react';
import cn from 'classnames';
import { Link } from 'react-router-dom';
import { Button } from '../../components/Button';
import { useCurrentTheme } from '../../state/local';
import { getDarkColor } from '../../state/util';

const cards: OnboardingCardProps[] = [
  {
    title: 'Terminal',
    body: "A web interface to your Urbit's command line (the dojo).",
    button: 'Install',
    color: '#9CA4B1',
    href: '/leap/search/direct/apps/~zod/webterm'
  },
  {
    title: 'Landscape',
    body: 'A suite of applications to communicate on Urbit',
    button: 'Install',
    color: '#D1DDD3',
    href: '/leap/search/direct/apps/~zod/landscape'
  },
  {
    title: 'Bitcoin',
    body: ' A Bitcoin Wallet that lets you send and receive Bitcoin directly to and from other Urbit users',
    button: 'Install',
    color: '#F6EBDB',
    href: '/leap/search/direct/apps/~zod/bitcoin'
  }
  // Commenting out until we have something real
  // {
  //   title: 'Debug',
  //   body: "Install a debugger. You can inspect your ship's internals using this interface",
  //   button: 'Install',
  //   color: '#E5E5E5',
  //   href: '/leap/search/direct/apps/~zod/debug'
  // }
  // {
  //   title: 'Build an app',
  //   body: 'You can instantly get started building new things on Urbit.  Just right click your Landscape and select “New App”',
  //   button: 'Learn more',
  //   color: '#82A6CA'
  // }
];

if ('registerProtocolHandler' in window.navigator) {
  cards.push({
    title: 'Open Urbit-Native Links',
    body: 'Enable your Urbit to open links you find in the wild',
    button: 'Enable Link Handler',
    color: '#82A6CA',
    href: '/leap/system-preferences/interface'
  });
}

interface OnboardingCardProps {
  title: string;
  button: string;
  href: string;
  body: string;
  color: string;
}

const OnboardingCard = ({ title, button, href, body, color }: OnboardingCardProps) => (
  <div
    className="p-4 flex flex-col space-y-2 text-black bg-gray-100 justify-between rounded-xl"
    style={color ? { backgroundColor: color } : {}}
  >
    <div className="space-y-1">
      <h4 className="font-semibold text-black">{title}</h4>
      <p>{body}</p>
    </div>
    <Button as={Link} to={href} variant="primary" className="bg-gray-500">
      {button}
    </Button>
  </div>
);

interface OnboardingNotificationProps {
  unread: boolean;
}

export const OnboardingNotification = ({ unread }: OnboardingNotificationProps) => {
  const theme = useCurrentTheme();

  return (
    <section
      className={cn('notification space-y-2 text-black', unread ? 'bg-blue-100' : 'bg-gray-50')}
      aria-labelledby=""
    >
      <header id="system-updates-blocked" className="relative space-y-2">
        <div className="flex space-x-2">
          <span className="inline-block w-6 h-6 bg-orange-500 rounded" />
          <span className="font-semibold">Home</span>
        </div>
        <div className="flex space-x-2">
          <h2 id="runtime-lag">Hello there, and welcome!</h2>
        </div>
      </header>
      <div className="grid sm:grid-cols-2 md:grid-cols-3 gap-4">
        {
          /* eslint-disable-next-line react/no-array-index-key */
          cards.map((card, i) => (
            <OnboardingCard
              key={i}
              {...card}
              color={theme === 'dark' ? getDarkColor(card.color) : card.color}
            />
          ))
        }
      </div>
    </section>
  );
};
