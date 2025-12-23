import { getBespokeItems } from '@/lib/cms';
import Postcard from '@/components/Postcard';
import { notFound } from 'next/navigation';
import Image from 'next/image';
import styles from './Bespoke.module.css';

export async function generateStaticParams() {
    const items = getBespokeItems();
    return items.map((item) => ({
        slug: item.slug,
    }));
}

export default async function BespokePage({ params }: { params: Promise<{ slug: string }> }) {
    const { slug } = await params;
    const items = getBespokeItems();
    const item = items.find(i => i.slug === slug);

    if (!item) {
        notFound();
    }

    const currentIndex = items.findIndex(i => i.slug === slug);
    const prevItem = currentIndex > 0 ? items[currentIndex - 1] : items[items.length - 1];
    const nextItem = currentIndex < items.length - 1 ? items[currentIndex + 1] : items[0];

    const prevLink = `/bespoke/${prevItem.slug}`;
    const nextLink = `/bespoke/${nextItem.slug}`;

    // Bespoke Metadata: "Project Name x Year x Location" equivalent?
    // Item has: Title, Material, Year.
    // I'll format as: Top: Title. Bottom: Material x Year.
    const metadataTop = item.title;
    const metadataBottom = `${item.material} × ${item.year}`;

    return (
        <main className={styles.container}>
            {/* Drawings/Sketches Flanking */}
            {item.drawings && item.drawings[0] && (
                <div className={`${styles.drawing} ${styles.drawingLeft}`}>
                    {/* Sketch 1 */}
                    <Image src={item.drawings[0]} alt="Process Sketch" width={300} height={400} style={{ objectFit: 'contain' }} />
                </div>
            )}

            {item.drawings && item.drawings[1] && (
                <div className={`${styles.drawing} ${styles.drawingRight}`}>
                    {/* Sketch 2 if exists */}
                    <Image src={item.drawings[1]} alt="Process Sketch" width={300} height={400} style={{ objectFit: 'contain' }} />
                </div>
            )}

            {/* Central Postcard */}
            <div className={styles.postcardWrapper}>
                <Postcard
                    imageSrc={item.heroImage}
                    imageAlt={item.title}
                    metadataTop={metadataTop}
                    metadataBottom={metadataBottom}
                    prevLink={prevLink}
                    nextLink={nextLink}
                />
            </div>
        </main>
    );
}
