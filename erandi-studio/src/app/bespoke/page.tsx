import { getBespokeItems } from '@/lib/cms';
import { redirect } from 'next/navigation';

export default function BespokeRedirect() {
    const items = getBespokeItems();

    if (items.length > 0) {
        redirect(`/bespoke/${items[0].slug}`);
    } else {
        return <div>Coming Soon.</div>;
    }
}
