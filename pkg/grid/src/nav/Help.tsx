import React from 'react';
import cn from 'classnames';
import { Button } from '../components/Button';

interface Group {
  title: string;
  icon: string;
  color: string;
  link: string;
}

const groups: Record<string, Group> = {
  uc: {
    title: 'Urbit Community',
    icon: 'https://fabled-faster.nyc3.digitaloceanspaces.com/fabled-faster/2021.4.02..21.52.41-UC.png',
    color: 'bg-black',
    link: '/apps/landscape/~landscape/ship/~bitbet-bolbel/urbit-community'
  },
  discovery: {
    title: 'Group Discovery',
    icon: 'https://urbit.me/images/icons/icon-512x512.png',
    color: 'bg-green-300',
    link: '/apps/landscape/~landscape/ship/~rondev/group-discovery'
  },
  foundation: {
    title: 'Foundation',
    icon: 'https://interstellar.nyc3.digitaloceanspaces.com/battus-datsun/2022.6.10..21.28.21-urbit-inverted.png',
    color: 'bg-black',
    link: '/apps/landscape/~landscape/ship/~wolref-podlex/foundation'
  },
  forge: {
    title: 'The Forge',
    icon: '',
    color: 'bg-black',
    link: '/apps/landscape/~landscape/ship/~middev/the-forge'
  },
  tlonSupport: {
    title: 'Tlon Support Forum',
    icon: '',
    color: 'bg-yellow-500',
    link: '/apps/landscape/~landscape/ship/~bitpyx-dildus/tlon-support'
  }
};

const GroupLink = ({ group }: { group: Group }) => (
  <div className="flex justify-between items-center py-2">
    <div className="flex space-x-2 items-center">
      {group.icon === '' ? (
        <div className={cn('w-8 h-8 rounded', group.color)} />
      ) : (
        <img className="w-8 h-8 rounded" src={group.icon} alt={`${group.title} icon`} />
      )}
      <div className="flex flex-col">
        <span className="font-semibold">{group.title}</span>
      </div>
    </div>
    <Button variant="alt-primary" as="a" href={group.link} target="_blank">
      {' '}
      Open in Groups{' '}
    </Button>
  </div>
);

const Wayfinding = ({ tlonCustomer }: { tlonCustomer: boolean }) => (
  <div className="inner-section space-y-8">
    <span className="text-lg font-bold">
      Urbit{!tlonCustomer ? ' Support & ' : null} Wayfinding
    </span>
    <p>
      A community of Urbit enthusiasts, developers, and various Urbit-building organizations are on
      the network to guide you.
    </p>
    <p>
      For direct assistance with any urbit-related issues, bugs, or unexpected behavior, please
      contact <a href="mailto:support@urbit.org">support@urbit.org</a>.
    </p>
    <p>
      If you need help getting situated on the network, or figuring out what fun things you can do
      with your urbit, join the following groups:
    </p>
    <div className="flex flex-col space-y-2">
      <GroupLink group={groups.uc} />
      <GroupLink group={groups.discovery} />
    </div>
    <p>
      If you are a developer and want to learn more about building applications for Urbit, check out
      these groups:
    </p>
    <div className="flex flex-col space-y-2">
      <GroupLink group={groups.foundation} />
      <GroupLink group={groups.forge} />
    </div>
  </div>
);

export const Help = () => {
  const tlonCustomer = !!window.URL.toString().indexOf('tlon.network');
  return (
    <div className="flex flex-col space-y-4">
      {tlonCustomer ? (
        <div className="inner-section space-y-8">
          <span className="text-lg font-bold">Tlon Customer Support</span>
          <p>
            As a customer of Tlon, you’re able to receive 24/7 support from the{' '}
            <span className="font-bold">Tlon Support Forum</span>, or you can email us at{' '}
            <a href="mailto:support@tlon.io">support@tlon.io</a>.
          </p>
          <GroupLink group={groups.tlonSupport} />
        </div>
      ) : null}
      <Wayfinding tlonCustomer={tlonCustomer} />
    </div>
  );
};
