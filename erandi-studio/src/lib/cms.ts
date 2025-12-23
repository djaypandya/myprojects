import projectsData from '@/data/projects.json';
import bespokeData from '@/data/bespoke.json';

export type Project = {
    id: string;
    slug: string;
    category: 'residential' | 'commercial';
    title: string;
    year: string;
    location: string;
    images: string[];
};

export type BespokeItem = {
    id: string;
    slug: string;
    title: string;
    material: string;
    year: string;
    heroImage: string;
    drawings: string[];
};

export const getProjects = (category?: 'residential' | 'commercial'): Project[] => {
    const projects = projectsData as Project[];
    if (category) {
        return projects.filter(p => p.category === category);
    }
    return projects;
};

export const getProjectBySlug = (slug: string): Project | undefined => {
    const projects = projectsData as Project[];
    return projects.find(p => p.slug === slug);
};

export const getBespokeItems = (): BespokeItem[] => {
    return bespokeData as BespokeItem[];
};
